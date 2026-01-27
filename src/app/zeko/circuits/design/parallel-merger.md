# Parallel merger

This document describes the parallel merger used by the sequencer to merge
transaction SNARKs and to coordinate commits.

## Abstraction

The merger is a generic library that does not know about Zeko-specific logic.
It is instantiated with four modules:

```ocaml
module Context : sig
  type t
end

module Base : sig
  type t
  val process : Context.t -> t -> Merge.t Deferred.t
end

module Merge : sig
  type t
  val process : Context.t -> t -> t -> t Deferred.t
end

module Commit : sig
  type t
  type out
  val process : Context.t -> t -> Merge.t -> out Deferred.t
end
```

The sequencer supplies the real implementations (e.g., `Base.process` proves a
transaction SNARK, `Merge.process` merges two proofs, and `Commit.process`
constructs and submits the L1 commit).

## Data structure

A _forest_ of _trees_ tracks batches to be committed. Each tree is a list of
jobs; only actionable jobs are kept in the list.

```ocaml
module Job_status = struct
  type t = Todo of Available_job.t | Done of Merge.t
end

type tree =
  { jobs : Job_status.t list
  ; closed : bool
  ; finished : Merge.t Ivar.t
  ; ready_to_commit : unit Ivar.t
  ; base_jobs : int
  }
```

Two rules define the algorithm:

1. If there is a `Todo` job, process it.
2. If there are two adjacent `Done` jobs, merge them into a new `Todo Merge`.

## Adding a base job

When a base witness is added:

1. Append a `Todo (Base witness)` job to the current tree.
2. Run `Base.process` asynchronously.
3. Mark the job as `Done` and check if a merge opportunity exists.
4. If two adjacent `Done` jobs exist, create a new `Todo Merge` job and process
   it; repeat until no merge opportunity remains.

## Committing

The sequencer can request a commit at any time:

1. Close the current tree (no new jobs can be added to it).
2. Create a new tree for subsequent base jobs.
3. Wait for all earlier trees to finish (commit ordering).
4. Wait for the closed tree to reduce to a single `Done` job.
5. Call `Commit.process` with the commit witness and the final merged proof.

## Persistence and restart

`Parallel_merger.Persisted` wraps the in-memory merger with a DB-backed queue:

- Each base witness is serialized to JSON and stored in a `parallel_merger`
  table with its `tree_id`.
- On restart, `create_and_requeue` loads stored witnesses and replays them into
  the in-memory merger, merging them into a single tree.
- After a successful commit, the corresponding tree is removed from the DB.

This ensures that sequencer restarts do not lose proving progress and that
commit ordering is preserved.
