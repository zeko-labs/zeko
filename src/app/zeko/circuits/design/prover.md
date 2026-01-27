# Prover service and message queue

This document describes the prover service used by the sequencer and how jobs
flow through the message queue.

## High-level flow

- The sequencer produces witnesses for circuit proofs.
- It sends a JSON-serialized request to the prover message queue.
- A prover worker consumes the request, runs the corresponding circuit prover,
  and replies with a JSON-serialized output.
- The sequencer uses the output (proofs and/or account update call forests) to
  advance the parallel merger and to construct commits.

## Message queue

The prover uses RabbitMQ (AMQP) via the `Message_queue` module.

## Prover input/output

The prover input is a JSON sum type that covers all supported witnesses.
The output is a JSON sum type with corresponding variants. This is required
because `yojson` does not support the GADT encoding of all variants.

## Prover worker

`Zeko_prover.Prover.run`:

1. Compiles circuits on startup.
2. Starts a message-queue worker.
3. For each message, decodes JSON -> `Input.t`.
4. Runs the corresponding prover.
5. Encodes `Output.t` -> JSON and responds.

Errors are caught and returned as `Output.Error` so the sequencer can retry or
surface the failure.

## Prover client

`Zeko_prover.Client` wraps the message queue with retry logic:

- Retries failed sends up to 5 times with 1s delay.
- Tracks queue size (single-producer counter).
- Supports priority jobs.

It also optionally caches action-state-extension (ASE) proofs in Postgres:

- `ase_cache_with_length` keyed by `(source_hash, target_hash)` plus length.
- `ase_cache_without_length` keyed by `(source_hash, target_hash)`.

This cache is an optimization only; the system remains correct without it.

## Testing hooks

The prover can be run with `--fake-proving-time` to simulate long proofs without
spending CPU. This is used in fake/test setups.
