# Data Availability Layer

## Node Description

The data availability multisig, as explained here #12, is a very simple idea and previous implementation was extremely complex. We don't need EVM nor the blockchain for executing the simple multisig architecture. In the new simplified solution of DA committee, each node is a standalone service, which on-request stores and signs the batch. The signature is an attestation that the node knows and has the information on how to reproduce the ledger.

The interface of the node API is roughly the following (`t` is the DA data unit):

```ocaml
type t =
      { source_ledger_hash : Ledger_hash.t
      ; target_ledger_hash : Ledger_hash.t
      ; diff : (int * Account.t) list
      ; command_with_action_step_flags :
          (User_command.t * bool list) option
      }

val post_batch : ledger_openings:Sparse_ledger.t -> batch:t-> Signature.t
val get_batch : Ledger_hash.t -> t option
```

Each data batch has the all the account changes stored in the `diff`. The da node applies each account's diff to the ledger openings, and compares the resulting ledger hash with the `target_ledger_hash`. If the adversary would want to lie, and therefore convince node to sign the ledger hash that the node doesn't know how to construct, the diff would need to be either incorrect, or some would be missing. To ensure neither happens following steps are taken:

1. Check that `root ledger_openings = batch.source_ledger_hash`.
2. Check that `batch.source_ledger_hash` is either in the database or an empty ledger.
3. Check that the indices in `batch.diff` are unique.
4. Set each account in `batch.diff` to the `ledger_openings` and call the resulting ledger root `target_ledger_hash`.
5. Sign `target_ledger_hash`.
6. Store the batch under the `target_ledger_hash`.

For convenience, we store also the full command, which helps to not only reconstruct the ledger, but also have to full history of the state. To ensure that the command is a correct one we do one additional step:

- Check that after applying all the receipts of the command, the receipt chain hashes match the target ledger.

The receipt chain hash is a commitment to all the transaction commitments. Transaction commitment doesn't include authorization fields, so the adversary can lie about the authorization of the corresponding command, this however doesn't allow to convince the da node to sign the incorrect ledger hash.

The api is exposed as `Async.Rpc` which is a typesafe ocaml rpc with binprot serialization. It's supposed to be very fast and reliable.

### Running the node

```bash
MINA_PRIVATE_KEY=<opaque> dune exec ./cli.exe -- run-node --port 8002 --da-node-to-sync http://localhost:8000 --da-node-to-sync http://localhost:8001
```

To see all the options run:

```bash
dune exec ./cli.exe -- run-node --help
```

## Client description

The client of the da node in our case is the sequencer. The sequencer posts each transaction to the every da node and at the event of the state commitment to the L1 takes the signatures of the latest ledger hash as the DA attestation. There's one requirement for the sequencer to meet, the transactions need to come in a strict order. If the transactions with order A, B would come in reversed order, the node would refuse the second transaction as it would not be able to reconstruct the previous state. To do that we use `Da_layer.Client.Sequencer` module, which enqueues all the transactions and makes sure that they are posted in correct order. In the event of commitment the sequencer asks the particular signature and `Da_layer.Client.Sequencer` returns a `Deferred.t` which gets filled once the signatures are present.
