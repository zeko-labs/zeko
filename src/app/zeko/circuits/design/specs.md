# Pseudo-code specs (unsynchronized, incorrect)

Rough OCaml-y specs can be found in this directory.
You are meant to read the account update generating functions
as generating the account updates you are meant to use
to interact with the system.
Any account update not generated for the zkapps here is meant to be invalid.
Thus, you can infer the circuit behavior from this.
Anything that is impossible to check, e.g. the parent account update,
is simply not part of the circuit.
In general we use X instead of hash of X.
Hash of X is implied where it's not feasible to use X itself,
(e.g. storing the ledger in the app state of an account).

We don't fill out all the fields for account updates.
Assume an implicit `with default`.
