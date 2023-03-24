# Mina zkApp: Mina Rollup by dcSpark

## Initial plan & thoughts

All the information here is tentative and quite likely to change.

The goal is to make make a minimal (first centralised, yet trustless) rollup
that is _fully isomorphic_ with L1 Mina, that is, all existing zkApps should be
deployable as-is on the rollup.

How do we achieve this?

Prior art: https://github.com/Trivo25/mina-zk-rollup (see xmas-sprint branch)

A very big non-goal is to _avoid reimplementing Mina's ledger_.
We want to reuse the existing code to 1) reduce the initial work but 2)
also ensure _that we are up-to-date_ with new changes to Mina, preventing us from
having to keep up with Mina.

### Handling of L2 state in L1 zkApp

Assume we are able to reuse the existing OCaml code, then the question is, how do we
maintain the Merkle tree of the state of the rollup on L1?
We were interested in using [zkfs](https://zkfs.io), however, the existing Mina rollup
implementation already has an ad-hoc Merkle tree library, eliminating the need for this
if we are to fork it.

But it is not clear whether we should fork it, as mina-zk-rollup reimplements much of the ledger.
If we strip this out of mina-zk-rollup, what code is there left to reuse, and is it worth it
rather than starting from scratch using zkfs? It is not clear.

### Reuse of OCaml code

Albeit I am still understanding the codebase (and learning OCaml), it seems to me that
we can interpret the ledger rules in any way we want by changing the modules in scope,
since it seems that the ledger rules are defined using an eDSL.

We can do the dumb and naive approach of possibly translating the rules into SnarkyJS code,
but that seems too dumb as the ledger rules are already in a format more amenable to being
transformed into a circuit.

As for integrating this with the code we write in SnarkyJS, this is not yet entirely clear.

In general, it is likely that we will in many parts essentially mimick the way Mina works
in the rollup, essentially making a proof of block validity, then verifying this recursively
using Pickles in the L1 zkApp. Our SnarkyJS code would have to verify this proof, but Mina
already has code for this too! The code we primarily need to write for the MVP is the handling
of the state, i.e. the Merkle tree, and the bridging between the L1 and L2.
This Merkle tree is essentially the same as the protocol state hash for the L1,
and possibly some extra data in the same Merkle tree for information necessary
for the bridging, or possibly in a separate Merkle tree.

Perhaps recursively proving verification of another proof that we proved the verification of the proof for the L2 state
validity is easier in terms of reusing code (i.e., proof A of proof B of deeply recursive proof C,
next block we have proof A' of proof B' of proof C', and C' proves C too, but not A and B. A is what is posted
to L1, i.e. zkApp execution. A and B could theoretically be merged, in which case AB would be proof of
zkApp execution of verifying proof of protocol state.).

## Other stuff

This is in addition to what is already noted in the litepaper.

We probably want to steal some of the ideas from [The Espresso Sequencer](https://hackmd.io/@EspressoSystems/EspressoSequencer).
- [HotStuff-2](https://eprint.iacr.org/2023/397)
- [Information Dispersal with Provable Retrievability for Rollups](https://eprint.iacr.org/2021/1544)

Considering the performance of Pickles/Kimchi, maybe we want to use FRI in the rollup (FRI backend for SnarkyJS?),
then prove the FRI proof on L1? Proof-sizes on L2 don't matter as much after all.

Other paper relevant for consensus: https://eprint.iacr.org/2022/1448

It is still worth looking into Celestia etc.
