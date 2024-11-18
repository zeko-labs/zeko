# Parallel merger

## Abastraction

This is a library used by sequencer to parallelize the merging of transaction snarks.
It tries to abstract away from the actual logic of sequencer, so it accepts 3 modules as arguments into the functor.
All of them have `type t` that represents a witness and `process` function that takes a witness and returns a deferred result.

Context is a module that is used by sequencer to provide handles to the e.g. `da_client` or `prover_client` and also takes care of persisting and restarting the proving process in case of a sequencer restart.

```ocaml
Context : sig
  type t
end
```

```ocaml
Merge : sig
  type t

  val process : Context.t -> t -> t -> t Deferred.t
end
```

```ocaml
Base : sig
  type t

  val process : Context.t -> t -> Merge.t Deferred.t
end
```

```ocaml
Commit : sig
  type t

  val process : Context.t -> t -> Merge.t -> unit Deferred.t
end
```

## Inner workings

```ocaml
module Tree = struct
  type t =
    { mutable jobs : Job_status.t With_id.t list
    ; mutable closed : bool  (** No new jobs can be added *)
    ; finished : Finished_job.t Ivar.t  (** All jobs are done *)
    ; ready_to_commit : unit Ivar.t  (** All jobs are done and ready to commit *)
    }
end

module Forest = struct
  type t = { mutable trees : Tree.t list }
end
```

Proving of transactions is a forest of trees, where each tree consists of one batch of transactions to commit.
The tree is represented by a list of jobs, we care only about actionable jobs and not the nodes that have already been proven or don't have a witness yet.

Job status can be either `Todo` or `Done`.

```ocaml
module Available_job = struct
  type t = Base of Base.t | Merge of Merge.t * Merge.t
end

module Finished_job = struct
  type t = Merge.t
end

module Job_status = struct
  type t = Todo of Available_job.t | Done of Finished_job.t
end
```

The whole logic implemented revolves around 2 rules:

1. If there is a `Todo` job, process it.
2. If there are 2 `Done` jobs next to each other in one tree, merge them.

## Adding a job

```ocaml
let add_job t ctx ~(data : Base.t)
```

Adding a job creates a `Todo Base` job at the end of a last tree and calls a process function.
After completion it is transformed into a `Done Merge` job by `finish_job_exn` function, and checks if it created an opportunity to merge.
Opportunity to merge are two consecutive `Done Merge` jobs in one tree.
Finished job can create only one opportunity to merge. If there is one, create new `Todo Merge` job and process it.

Additionally all the jobs have unique id, so we can track them.

```ocaml
module With_id = struct
  type 'd t = { id : string; value : 'd } [@@deriving sexp, yojson]
end
```

## Committing

```ocaml
let commit t ctx ~commit_witness
```

Sequencer can commit a tree at any time. Committing in proving context means that current tree should be closed (no new jobs added) and once it's all proven it should be committed.
Closing a tree means that there needs to be a new tree created where all the new jobs will go to.
Commit function will:

1. Close the last tree and create a new one.
2. Wait for the previous jobs to finish (because of the order of commits).
3. Wait until there is only one `Done Merge` job left in the list.
4. Calls `Commit.process`
