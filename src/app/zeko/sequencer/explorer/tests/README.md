# Explorer Gherkin Tests

These tests cover the explorer-facing behavior added by the sequencer NATS and
backfill work. The feature files stay in Gherkin so the behavior is readable,
while the OCaml harnesses execute those scenarios against the in-repo modules
and, where needed, a real NATS server.

## Explorer Gherkin Integration Suite

There is one explorer Gherkin integration suite with two execution categories
for now.

### In-process scenarios

File:

- `explorer_gherkin_tests.ml`

This is an inline-test library. It checks behavior that does not require an
external broker or service process:

- explorer event payload and header contracts
- backfill GraphQL schema behavior
- GraphQL-SSE progress stream formatting
- sequencer replay genesis classification

Run it directly with:

```bash
opam exec -- env -u DUNE_RPC dune runtest --profile=devnet src/app/zeko/sequencer/explorer/tests
```

This command also runs the broker-backed scenarios when `NATS_URL` points at a
running NATS server, because the tests directory wires both current entry points
into the Dune `runtest` alias.

### Broker-backed scenarios

File:

- `explorer_nats_gherkin_tests.ml`

This is a standalone Async executable because it connects to a real NATS server.
It checks that the shared explorer publisher paths produce messages that a real
subscriber receives with the expected subject, payload, and `Nats-Msg-Id`
header. It also checks the Zeko JetStream stream contract by publishing the
same transaction message twice with the same `Nats-Msg-Id` and asserting the
stream stores one message.

Run it separately with:

```bash
NATS_URL="nats://127.0.0.1:4222" \
  opam exec -- env -u DUNE_RPC dune exec --profile=devnet \
  src/app/zeko/sequencer/explorer/tests/explorer_nats_gherkin_tests.exe
```

Run the whole explorer suite with NATS enabled:

```bash
NATS_URL="nats://127.0.0.1:4222" \
  opam exec -- env -u DUNE_RPC dune runtest --profile=devnet \
  src/app/zeko/sequencer/explorer/tests
```

The `Explorer Gherkin` CI workflow uses this full-suite command and provides
NATS through a GitHub Actions service container with JetStream enabled.

## Adding Coverage

Keep scenarios declarative: describe the explorer behavior, not the OCaml
implementation steps. Add one scenario per behavior.

Use in-process scenarios for deterministic behavior that does not need external
infrastructure. Use broker-backed scenarios when the behavior depends on NATS
delivery, subjects, headers, or JetStream storage semantics.

## Process-level E2E Scenarios

File:

- `explorer_e2e_gherkin_tests.ml`

Process-level end-to-end coverage is a separate, slower Gherkin entry point
rather than part of the fast integration suite. It should boot real services
and assert behavior from the outside where practical:

- start the sequencer with `--nats-url`, submit a transaction through the
  sequencer API, and assert a NATS subscriber receives `zeko.l2.transactions`
- start the backfill HTTP server with a DA fixture and real NATS, call the
  GraphQL `backfill` mutation over HTTP, and assert both GraphQL-SSE progress
  and replayed NATS messages

The current E2E entry point reuses the existing sequencer integration service
setup: real L1 test ledger, DA nodes, signers, RabbitMQ, Postgres, and NATS.
The sequencer runtime is still driven through `Sequencer.create`, matching the
existing sequencer integration tests. A later follow-up can move this to a
fully external `run.exe` process once the bootstrap/deploy path is ready for
that shape.

Unlike the shared sequencer harness path, the `explorer-e2e` mode in
`run-sequencer-test.sh` still builds its own dedicated test executable and
service binaries because the standalone `Explorer E2E` workflow does not run a
separate prebuild step first.

Run it with:

```bash
NATS_URL="nats://127.0.0.1:4222" \
  ./src/app/zeko/sequencer/tests/run-sequencer-test.sh real 1 explorer-e2e
```

The E2E harness should remain separate from the faster integration suite so
failures identify whether the break is in the in-process behavior, NATS broker
delivery, or full service orchestration.
