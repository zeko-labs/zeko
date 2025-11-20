
<!-- DSL DESIGN SECTION -->
#### DSL Semantics

- error accumulation
- leftover nodes count as errors (ensures crashes are bundled as errors or are manually expected)
- analysis steps extraction
  - assertion steps
  - collection steps

##### Testing Phases

Running a test occurs in two distinct phases: the execution phase, and the analysis phase.

###### Execution

The execution phase is in which, as the name implies, the test is actually executed. This includes spawning nodes through the orchestrator, communicating with nodes to execute the test, and some runtime assertions which will stop the test (which are mostly hidden inside DSL primitives). The execution phase continues until the test DSL has been fully interpreted. If the test is stopped prematurely, the execution phase will skip the analysis phase and will merely tear down the orchestra and provide all errors accumulated thus far.

###### Analysis

The analysis phase occurs at the end of a test run. In this phase, the test executive consumes logs and metrics collected from nodes during the test and checks for various conditions asserted during the test. At the conclusion of this phase, any dangling members of the orchestra are cleaned up.

##### Error Accumulation

Two different levels of errors can occur when running a test: non-fatal errors, and fatal errors. Most errors when running a test are considered non-fatal. For instance, if the test asserts something happens and it does not, this is considered a non-fatal error by default since the rest of the test can continue executing and discover more errors. Similarly, a hitting soft-timeout when waiting for a condition is a non-fatal error, so the test will continue running if the condition triggers before a hard-timeout, helping to identify regressions or recently invalidated or even flakey timeouts. An example of a non-fatal error, besides the hard-timeout example stated previously, is when the test attempts to interact to a node which has crashed. By comparison, for another example of the non-fatal/fatal error division, destroying a node which has crashed is non-fatal. Fatal errors, by comparison, will immediately fail the test. Fatal errors can only occur during the execution phase of a test; any errors during the analysis phase are considered non-fatal. Non-fatal errors are accumulated as the test is run and are emitted at the end of the test. For obvious reasons, the definition of a "passing" test is a test which does finishes successfully without accumulating any non-fatal errors.

TODO: error severity ordering? seems tricky to get right, but super useful

<!-- DSL DESIGN SECTION -->
#### DSL Primitives

##### Decorations

```ocaml
val section : string -> unit Malleable_error.t -> unit Malleable_error.t
val section_hard : string -> 'a Malleable_error.t -> 'a Malleable_error.t
```

The `section` function associates a portion of DSL code with a name. This is useful for tracking the high level of "why" the test is doing what it is doing. These names get propagated to failures and accumulated errors within the `Malleable_error.t` context. `section_hard` is a variant for sections that produce a value.

##### Orchestra Interactions

Node lifecycle and network orchestration are handled by the engine and network layers rather than DSL primitives. Tests interact with networks via `Engine.Network_manager` and `Engine.Network`, and individual nodes via `Engine.Network.Node.start/stop`. There are no `spawn`/`destroy` functions in the current DSL.

##### Concurrent Tasks

Concurrent task primitives (`task`, `stop`) are not part of the current DSL surface.

##### Synchronization

```ocaml
val wait_for : t -> Wait_condition.t -> unit Malleable_error.t
```

Waiting is expressed via `Wait_condition` constructors and optional timeouts:

```ocaml
module Wait_condition : sig
  type t
  val blocks_to_be_produced : int -> t
  val block_height_growth : height_growth:int -> t
  val nodes_to_synchronize : Engine.Network.Node.t list -> t
  val node_to_initialize : Engine.Network.Node.t -> t
  val nodes_to_initialize : Engine.Network.Node.t list -> t
  val signed_command_to_be_included_in_frontier :
    txn_hash:Mina_transaction.Transaction_hash.t ->
    node_included_in:[ `Any_node | `Node of Engine.Network.Node.t ] -> t
  val ledger_proofs_emitted_since_genesis :
    test_config:Test_config.t -> num_proofs:int -> t
  val zkapp_to_be_included_in_frontier :
    has_failures:bool -> zkapp_command:Mina_base.Zkapp_command.t -> t
  val persisted_frontier_loaded : Engine.Network.Node.t -> t
  val transition_frontier_loaded_from_persistence :
    fresh_data:bool -> sync_needed:bool -> t
  val with_timeouts :
    ?soft_timeout:Network_time_span.t ->
    ?hard_timeout:Network_time_span.t -> t -> t
end
```

##### Analysis

```ocaml
val watch_log_errors :
  logger:Logger.t ->
  event_router:Event_router.t ->
  on_fatal_error:(Logger.Message.t -> unit) ->
  log_error_accumulator

val lift_accumulated_log_errors :
  ?exit_code:int -> log_error_accumulator ->
  Test_error.remote_error Test_error.Set.t
```

Assertions and metric collection are handled via log/event processing (`Log_engine`, `Event_router`, `Network_state`). There is no `assert` or `collect` DSL primitive in the current surface.

##### Node Interactions: TODO

Direct node interactions are performed via `Engine.Network` and `Engine.Network.Node` APIs rather than dedicated DSL primitives.

<!-- DSL DESIGN SECTION -->
#### Pseudo-code Examples

Note: The following examples reflect an earlier design proposal and do not match the current DSL API. They should be translated to use `Engine.Network_manager`/`Engine.Network` for orchestration and `Dsl.wait_for` with `Wait_condition` for synchronization.

##### Basic Bootstrap Test

```ocaml
let genesis_ledger = Genesis_ledger.of_balances [2_000] in
let config =
  { genesis_ledger
  ; proof_level= `Check
  ; slot_time= 20_000
  ; k= 6
  ; delta= 2 }
in
let args ?seed ?proposer () =
  { default_args with
    seed
  ; proposer_keypair= Option.map proposer (Genesis_ledger.get_keypair genesis_ledger) }
in
let test_bootstrap bootstrap_type expected_slots =
  section bootstrap_type (
    let%bind bootstrapping_node = spawn config (args ~seed ()) in
    let%bind () = wait_for ~status:`Synced ~timeout:(`Slots expected_slots) () in
    let%bind () = collect ~name:bootstrap_type ~node:bootstrapping_node ~metric:`Bootstrap_time () in
    destroy bootstrapping_node)
in
let%bind seed = spawn config (args ())
let%bind node1 = spawn config (args ~seed ~proposer:_0 ()) in
let%bind () = wait_for ~blocks:config.k () in
let%bind () = test_bootstrap "bootstrap after k blocks" 2 in
let%bind () = wait_for ~epoch_reached:2 () in
let%bind () = test_bootstrap "bootstrap after 2 epochs" 6 in
let%bind () = destroy seed in
destroy node1
```

##### Partition Rejoin Test

```ocaml
(* helper for performing peer actions across an entire partition *)
let partition_action partition peers ~f =
  DSL.List.iter partition ~f:(fun node ->
    DSL.List.iter peers_to_ban ~f:(fun peer ->
      f node ~peer))
in

let partition_size = 6 in
let genesis_ledger = Genesis_ledger.of_balances (List.make (partition_size * 2) ~f:(Fn.const 20_000)) in
let config =
  { genesis_ledger
  ; proof_level= `Check
  ; slot_time= 20_000
  ; k= 6
  ; delta= 2 }
in

(* spawn the network, fully connected *)
let%bind seed = spawn config default_args in
let spawn_proposer i =
  let proposer_keypair = Genesis_ledger.get_keypair genesis_ledger i in
  spawn config {default_args with seed; proposer_keypair}
in
let%bind nodes =
  DSL.List.map ~f:spawn_proposer
    (List.make (partition_size * 2) ~f:Fn.id)
in
let left_partition, right_partition = List.partition nodes partition_size in

(* TODO: assert that there is a minimum amount of peers on every box *)

(* wait until the frontier is full *)
let%bind () = wait_for ~blocks:config.k () in
(* this has an implied soft timeout of (`Slots (8 * config.k)) *)

(* disconnect the partitions by updating the blacklists on both sides *)
let%bind () = partition_action ~f:blacklist_peer left_partition right_partition in
let%bind () = partition_action ~f:blacklist_peer right_partition left_partition in

(* kill the seed node so that there is no bridge between the partitions *)
let%bind () = destroy seed in

(* wait for 2 finalizations while the networks are disconnected *)
let%bind () = wait_for ~blocks:(config.k * 2) () in

(* add blacklisted peers to the whitelist on both sides *)
let%bind () = partition_action ~f:whitelist_peer left_partition right_partition in
let%bind () = partition_action ~f:whitelist_peer right_partition left_partition in

(* wait for the 2 partitions to rejoin *)
let%bind () =
  wait_for
    ~common_prefix:((`Nodes (left_partition @ right_partition), `Distance config.delta))
    ~timeout:(`Blocks config.k)
    ()
in

(* tear down the network *)
DSL.List.iter (left_partition @ right_partition) ~f:destroy
```

ALTERNATIVE TEST: keep two partitions separate with a fixed topology, with 1-2 intermediate
nodes bridging the networks, then take the other bridge offline temporarily and then have them
rejoin the network without topological restrictions and see if the chains reconverge

##### Basic Hard Fork Test

The following example is incomplete and needs more work to think about how the testing DSL would work with multiple deployment artifacts which it has to be compatible with at once.

```ocaml
(* NOTE: pretty sure I will kill the Vect and Peano GADT stuff *)
let genesis_ledger =
  Genesis_ledger.of_balances
      (* 3 proposers *)
    [ 20_000; 20_000; 20_000
      (* 2 snark workers *)
    ; 1_000; 1_000
      (* txn sink *)
    ; 0 ]
in
let config =
  { genesis_ledger
  ; proof_level= `Full
  ; slot_time=180_000
  ; k=6
  ; delta=2 }
in

(* create the network *)
let%bind seed = spawn config (args ())
let proposer_args n = {default_args with seed; proposer_keypair= Genesis_ledger.get_keypair genesis_ledger n} in
let snarker_args n = {default_args with seed; snarker_keypair= Genesis_ledger.get_keypair genesis_ledger n} in
let node_args = Vect.[proposer_args; proposer_args; proposer_args; snarker_args; snarker_args] in
let nodes = Vect.mapi node_args ~f:(fun i args -> spawn config (args i)) in

(* send payments continually until the snarked ledger transitions *)
let%bind txn_task =
  task (fun txn_task ->
    let i, sender_node = Vect.randomi nodes in
    let%bind () =
      times 10 (
        send_user_command
          ~sender_node
          ~command:(`Payment 1)
          ~from:(Genesis_ledger.get_public_key genesis_ledger i)
          ~to:(Genesis_ledger.get_public_key genesis_ledger (Vect.length nodes)))
    in
    let%bind () = wait_for ~slots:1 () in
    txn_task)
in
let%bind () = wait_for ~snarked_ledger_commits:1 ~timeout:(`Epochs 6) () in
let%bind () = stop txn_task in

(* this is tricky... we need 2 builds so we can distribute the second one... *)
let%bind () = schedule_hard_fork ~node:seed .... in
```

##### TODO: Concurrency Example

