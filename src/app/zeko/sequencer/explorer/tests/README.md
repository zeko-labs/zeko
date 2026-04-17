# Explorer Gherkin Tests

These tests cover the explorer-facing behavior added by the sequencer NATS and
backfill work. The feature files stay in Gherkin so the behavior is readable,
while the OCaml harnesses execute those scenarios against the in-repo modules
and, where needed, a real NATS server.

## Harnesses

There are two harnesses for now.

### Contract Gherkin harness

File:

- `explorer_gherkin_tests.ml`

This is an inline-test library. It checks explorer contracts that do not
require an external process:

- explorer event payload and header contracts
- backfill GraphQL schema behavior
- GraphQL-SSE progress stream formatting
- sequencer replay genesis classification

Run it directly with:

```bash
opam exec -- env -u DUNE_RPC dune runtest --profile=devnet src/app/zeko/sequencer/explorer/tests
```

This command also runs the NATS integration harness when the tests directory
wires both harnesses into the Dune `runtest` alias.

### NATS Integration Gherkin harness

File:

- `explorer_nats_gherkin_tests.ml`

This is a standalone Async executable because it connects to a real NATS server.
It checks that the shared explorer publisher paths produce messages that a real
subscriber receives with the expected subject, payload, and `Nats-Msg-Id`
header.

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
NATS through a GitHub Actions service container.

## Adding Coverage

Keep scenarios declarative: describe the explorer behavior, not the OCaml
implementation steps. Add one scenario per behavior.

Use the contract harness for deterministic module-level behavior that does not
need external infrastructure. Use the NATS integration harness when the behavior
depends on NATS delivery, subjects, or headers.

Process-level end-to-end coverage should be added as a third, slower Gherkin
harness rather than folded into the fast contract harness. That harness should
boot real services and assert behavior from the outside:

- start the sequencer with `--nats-url`, submit a transaction through the
  sequencer API, and assert a NATS subscriber receives `zeko.l2.transactions`
- start the backfill HTTP server with a DA fixture and real NATS, call the
  GraphQL `backfill` mutation over HTTP, and assert both GraphQL-SSE progress
  and replayed NATS messages

The process-level harness should run in CI, but it should remain separate from
the fast harnesses so failures identify whether the break is in the contract,
NATS publishing, or full service orchestration.
