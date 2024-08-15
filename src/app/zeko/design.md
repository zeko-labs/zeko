# Internal design of circuits

## Outline

We wish to have a rollup.
A nested instantiation of the Mina ledger on top of itself,
as a zkApp account.
We wish to support communication between the "outside" (L1) and "inside" (L2).
It should be possible to transfer value.
It should be possible to upgrade the contract when the time comes.
It should be possible to have a stake in the rollup.
And above all, it should be secure.

To that end, here is a summary of the core protocol:
- The core rollup protocol does not handle transfer of value/MINA.
- There is a sequencer auction on L1.
- There is an associated token called ZEKO, using the fungible
  token standard, minted on the L1.
- Auctions are bid in ZEKO.
- People pay fees in whatever currency they like to sequencer.
- Data availability is ensured by having a public key that must have
  sign the hashes used.
- Communication happens by posting actions to the zkApp account on the L1.
  This account is referred to as the _outer_ account.
  There is a corresponding _inner_ account on the L2,
  whereto you can post actions to communicate the other way.
  On commit, the action states are synchronised.
  Notably, the outer action state is synchronised _up to some point_,
  to ensure that sequencer does not waste work synchronising something that
  might be rolled back immediately.
- Actions on the outside:
  + Witness (witness arbitrary account update)
  + Bid (sequencer bid)
  + Commit (sequencer committed)
  + Time (match time, can be emulated by Witness
    technically, included for efficiency of token transfer)
- Governance is done tallying votes based on how much ZEKO
  the voter holds on the L1 and L2 combined.
  Delegation is done by having a token account that
  has a field that can point to another public key
  (which might delegate recursively!) to delegate voting control to.
- There is a backup special committee that can pause the rollup.
  Being paused is indicated by a field on the outer account.

How can we implement transfers of tokens on top of this?
Consider the coremost MINA case:
There is a special account on the L2 that is initialized with maximum MINA.
There is a corresponding account the L1, to which you can deposit MINA.
You witness the deposit, and can thus correspondingly take out the MINA on the L2.
Double spending is prevented by tracking a token account on the L2 the index of
the last deposit processed.
The index is the index of the witness action on the outer account in the merkle list
of actions.
Withdrawals happen correspondingly, the other way around.
We also wish to support timeouts on deposits.
We do this by regarding a deposit as having three states:
- Unknown
- Accepted
- Rejected

It starts by default as Unknown. Iterate through all actions that come after,
and if there is a Time action where the lower bound is higher than the timeout,
it's marked as Rejected.
If there is a Commit action that commits an action state that contains the deposit
before it times out (marked by a Time action), then it's marked as Accepted.

We handle cancelled deposits the same way as withdrawals.
The index of the last cancelled deposit withdrawn is stored in a token account
on the L1 as with for withdrawals.

In addition, withdrawal logic must assert that the rollup is not paused.

## Synchronization

We wish to support transfer of information between L1 and L2.
To that end we have the special inner account.
The inner account has a field which contains the outer action state.

The outer circuit checks whether the inner app state field matches
a predecessor of the outer action state.
However, inner app state may have been updated many times.
Thus, inner circuit also checks that stored
outer action state only moves forward.
Correctness of each individual step is implied by this.

We check that it matches a predecessor instead of the actual action state
such that sequencer can avoid making SNARKs that are invalidated in the event
of a rollback.

Consider, however, the case where the sequencer does not move
the recorded outer action state forward enough.
This would mean communication is delayed (and is one reason why timeouts are
important!).
We wish to incentivize sequencers to move the recorded action state forward
as much as possible.

This is already the case, in fact, since as a sequencer you want to
reap in as many fees as possible.
To that end it is optimal to move the action state forward at the very beginning,
such that people can process their withdrawals, use their funds, and pay
the sequencer fees.

### FIXME: Store multiple action states? (#177)

We only store _one_ of the action states,
meaning that if you don't prove fast enough you could
miss your chance to submit your transaction.

## Sequencer election

Sequencer election works by doing an auction,
where the currency is ZEKO.

We must know who the sequencer is for some range of slots
well beforehand, such that it's finalized by then.
This is to prevent wasted work when proving.

Bids are published for a range of macroslots as an action,
along with a per macroslot price. The price is in ZEKO.

A macroslot is defined as N slots, for some N.

The bid is not atomic, that is, it is possible for only part
of the bid to be accepted.

The bid must be made no less than N slots before,
and no more than M slots before, for some N and M.

When doing a commit during the macroslot, it must be shown
that a corresponding bid was made, and that it was the winning
bid, by folding over all bids within the time frame (deduced
by checking slot bounds of surrounding actions).

If the sequencer makes a bid, but loses, they can get
the ZEKO they used back.
They can do this while posting another bid too,
before the macroslot bid for has passed.

There should not be any unbid slots,
since for any sequencer it would make sense
to submit a bid for all slots for minimum ZEKO.

If however, this should still happen,
then the macroslot is given to the sequencer
for the last macroslot.

### Sequencer slashing

There is a state of limbo when control
is transferred from one sequencer to the other.

The next sequencer needs to begin proving as soon as possible,
but needs to know the present state of the rollup
to begin proving.

They can not necessarily trust what the sequencer responds
with as the ledger hash to be committed,
since they can at the end of their turn submit new transactions
to the DA layer, and commit one of the resultant ledgers,
without telling anyone which directly.

The next sequencer can not begin proving, because there is more
than 1 possibility given the DA (in general, for N transactions,
N possibilities, not astronomical, but enough that it's infeasible.).

This problem is however made much worse when you consider the weak finality
of Mina, and the # of possible branches it can take.
What if there are 2 block producers, and the sequencer
presents a slightly different commit to each
(with a constant amount of extra proving work?).

What if multiple forks develop in parallel because of networking issues
in the network?

To prevent sequencers from committing to more than one ledger,
we have slashing.
In the event they commit to more than one ledger,
they will lose the amount equal to the ZEKO paid x N for some N.

Really, it should depend on the price of the next slot,
but this is much harder to do, because we also need to deposit
the funds we slash at some point, and that deposit needs to be
a reliable transaction that isn't rolled back either.

With this approach we can simply make the bid take N times the amount
the bid is for, making it easily slashible and refundible.

The funds used for slashing can be withdrawn at any point after the macroslot
once someone has made a new commit.

## Committing

On commit, the sequencer must present a transaction snark
that represents the transition from the old ledger hash
stored in the outer account to the new one.
It must also submit an action that contains the ledger hash
committed, along with the inner action state and processed outer
action state at the time of the commit.

Deposits (albeit external to the system) use this mechanism to
figure out if they've been rejected or not.

Thus, the commit must also include the slot range used at the time
of the commit.
If the slot range's upper bound is lower than the deposit's timeout,
then it must have been processed.

However, the sequencer doesn't always have any good incentive to choose
a tight bound, since choosing a tight bound means that _future_ sequencers
can profit from those deposits being processed.

To prevent this from happening, we also specify a maximum size for the slot range.

The commit can only happen if the sequencer has won the auction for the macroslot.

## ZEKO token (unimplemented)

The ZEKO token will use the
[fungible token standard](https://github.com/MinaFoundation/mina-fungible-token),
which, interestingly, is also an implementation.
This is to enable third parties to use the token without importing foreign code
to generate the proof; every token uses the same vk, so you only need to use known
code.

We can however choose a vk that controls minting.
In our case, the vk delegates control the the outer account,
and the current policy of the outer account is to disallow minting
entirely.

The allocation is for now fixed, and determined when the token is deployed.

## Forced account update / governance (unimplemented)

(NB: permissions on account might be set to Either,
so this isn't necessarily the only way of doing a forced update).

If more than 2/3 vote yes on an account update (by signing it),
we permit it to pass.
We know the total amount of ZEKO,
and can prove how much ZEKO a public key owns by
setting a network precondition on `staking_epoch_data`,
from which we get the ledger used for the current epoch.
Then we can open the ledger and check how much ZEKO each account
owns.
Notably, we do not need to check _all_ accounts,
since we already know how much the total is.
We only need to calculate enough to get 2/3.

Since we can't expect everyone to be online,
we also allow delegation.
Delegation works by setting the public key
in a helper account.
Since we can't know if someone delegates
without checking this account,
you are forced to set this,
but you can set it to yourself.
Delegation can also be recursive,
such that the delegatee can delegate again.

Thus, given a signature on the account update,
to prove its weight, you start with accounts that
delegate to it, and then accounts that delegate to
those, and so on.

We however special-case the outer account of the rollup itself,
and instead consider the ledger inside the outer account
as a second ledger, used in the same way as above.

FIXME: If DA fails right around the epoch switch,
then people who have their ZEKO inside Zeko will not be able to vote.
Figure out a good way of using the `staking_epoch_data` before
the current one.

## Our changes to transaction logic

We do not support processing failed transactions,
thus, you can not take fees from failed transactions.

Neither do we support any network preconditions currently.
We might support a subset in the future to allow checking the ledger hash.

We support time preconditions.
There is however not a single global slot when proving transactions.
There is a range, and that range (or a tighter one) must be used as precondition when committing.

The sequencer should however not accept transactions that have tight time bounds,
since the commit would be likely to fail, wasting work potentially.

## Emergency pause

There is a public key that can pause the rollup.
The public key is stored in the app state and can be changed via governance.

## Sequencer API

Sequencers should publish a GQL endpoint which users can use
to communicate with the sequencer.
This endpoint should at the very least support:
- Querying current ledger hash scheduled to be committed
- Posting transactions

Users can from the ledger hash get the current state of the ledger
from the DA layer.

There is no built-in way for users to find the sequencer.
Sequencers could post the IP address or domain name
as transaction metadata, they could use yggdrasil, etc.

It is in the interest of the sequencer not to publish this
information too early to avoid being DoSed.

Once the sequencer has "signed" the last commit for the macroslot,
users should send their commands to the next sequencer, who can then
begin proving.

## Notable missing features

- Forcing a transaction to happen, bypassing the sequencer,
  to e.g. initiate and process a withdrawal.

## Soundness

### Assumptions

- We assume an honest majority of ZEKO token holders
  (weighted by their stake) (somewhat reasonable).
- We assume the L1 is secure (reasonable).
- We assume the L1 has reliable finality after X blocks (reasonable).
- We assume that during a macroslot, not all block producers censor
  the sequencer.

### Censorship

The last one is especially dangerous.
Not only does the sequencer pay for the macroslot,
it's also possible that they will be slashed if they
don't commit at all.

Someone could bribe the block producers to censor the sequencer.
It's not clear if there's a good incentive for this though.

### MEV

The sequencer can also exploit MEV, but the amount they can exploit for
is reduced by the auction, any increase in revenue
would mean they have more to spend on the auction,
which they will if there is a competitive market.

### Cost of running a sequencer

Spinning a sequencer up and down is also not free,
so there is more profit if the sequencer has many slots
in a row, since they can utilize the hardware for longer.

### L1 forks

We want Zeko to have stability even if the L1 forks.
This is both good UX, but also necessary to avoid
making sequencers wasting work.

If during a macroslot, there are two possible states of Zeko,
the next sequencer has two possible states to build on top of,
doubling the amount of proving work they have to do if they
want to build on both forks.

If they knew which fork would win, this would not be an issue,
but they don't in the case of Nakamoto-style consensus
as used in Mina.

This is why we have slashing.

### Erroneous slashing

We don't want good actors to be slashed.
There is a danger of this happening when the sequencer
for the previous macroslot makes multiple commits,
since the commit made by the next sequencer may not be
valid in some of the forks.
FIXME: fix this.

### Sequencer API lying (to implement)

Currently the sequencer can lie about the current state of the rollup.
There is nothing forcing people to use the API instead of the DA layer
directly, but even then, they need to get the current ledger has from the DA.
We can solve this by having the sequencer sign in-progress ledger hashes,
along with a number to denote how many transactions have been applied.
The sequencer would then show that the response is correct by including
the opening into the Merkle tree that is the ledger, along with the signature.
If the sequencer lied, and committed a ledger which history did not include the
signed ledger, then they would be subject to slashing.
The sequencer could however still present an old version of the ledger
in the response.
It would nonetheless still be better than no guarantees at all.
FIXME: implement this.
This is also complicated by the fact that the sequencer might accept a transaction
which is later found out not to be provable due to discrepancies between
the code that accepts the transaction and the code that proves it.

### DoS attacks

It might be in the interest of other competing sequencers
to DoS the sequencer currently elected to prevent them from
participating in the auction.
This is more likely if the sequencers don't have a stake in Zeko,
in the form of ZEKO.
FIXME: enforce that sequencers have a stake in Zeko.

# Token transfer/bridge contract

The transferring/bridging of tokens is done in a separate contract
entirely.
The gist is that an outer account works as a bank,
which when deposited to, mints corresponding promissory
notes from a corresponding inner account.

These accounts are separate from the outer and inner account of the rollup.
We will thus refer to this new pair of accounts as the token outer and inner accounts.
For disambiguation purposes, the ones for the rollup are prefixed with rollup.

## Deposits

Deposits are made by posting an action on the rollup outer account,
along with sending the funds to the token outer account (verified by proof).
The user specifies an upper bound (slot) after which point if not
processed, the deposit will timeout and the funds will be recoverible.
The action will be a Witness action that witnesses the deposit to the
token outer account.

As explained above, on commit, the sequencer will also post an action on the
rollup outer account that details what kind of commit we made, along with the
slot bounds for the commit itself.

On the inside, the user can finalize a deposit that has been accepted.
A user deposit is accepted if there is a "commit" action after it
with suitable slot bounds such that the upper bound on the commit is less
than the timeout,
AND if there is no "time" action between them, which like a
"commit" action, is posted alongside its slot bounds, such that
the timeout is less than the lower bound of the "time" action.

We must however prevent double spends, thus the user must also provide
their helper account in the context (as a child),
and prove that they haven't processed it already.
This is done by storing in the account the index of the deposit last processed.
The index stored must be less than the index of the new deposit to be finalized.
After this, the index is updated to be the index of the new deposit.
Notably, it is possible to "skip" a deposit erroneously, but it is on the user
not to do this accidentally.

In this process we must match on the rollup outer action state as stored
in the inner account. This value can change, and cause the preconditions
to fail, but this is of no worry since failed transactions are feeless on Zeko.

The funds are then minted or sent by the token inner account,
depending on what kind of token it is.

## Withdrawals

To do withdrawals, we similarly post an action on the rollup inner action state.
We use the Witness action to show that we've either burned or sent the tokens
back to the token inner account.

To withdraw, a constant number of slots must have roughly passed since the
withdrawal was added. We figure out when a withdrawal was processed
by using the Commit actions added by the sequencer.
It does not matter which Commit action included it first.

Given this, the user can prove an upper bound for when their withdrawal was added
by taking some commit that includes their action and using its upper slot bound.
There is however an issue here:
As with the deposit case, we must match on the rollup inner action state stored on the
rollup outer account's app state.
This might change, invalidating the transaction.
Currently we don't work around this.
FIXME: fix #177.

As in the deposit case, we need to prevent double spends, thus similarly,
we have helper accounts on the outside too.
As with deposits, the helper account keeps track of the index of the last withdrawal
processed.
We also have to consider emergency changes.
The rollup inner action state might "roll back" and procede in another direction
due to this, and this is why we store the inner action state in the outer
account explicitly instead of just as an action.
This can happen via e.g. governance.
In addition to proving that it is contained in the rollup inner action state stored
in the rollup outer action state's "commit" action, we must prove that it's
also contained in the inner action state recorded in the outer app state.

## Cancelled deposits

There is however one more kind of transfer:
Cancelled deposits.
A cancelled deposit can be finalized analogously to deposits,
but on the outside, by proving that the action corresponding to the
deposit has been followed by an "time" action, which lower slot bound
exceeds the timeout slot, while no "commit" action which upper bound is less than
the timeout slot precedes it but comes after the deposit's action.

Analogously, to prevent double spends, we must keep track of this.
We use the same helper account as for withdrawals,
thus the helper account on the outside keeps track of two indices,
one for the index of the last withdrawal processed, and
one for the index of the last cancelled deposit processed.

## Governance (separate)

For a non-canonical bridge zkapp, governance is separate.
There is however the issue that the token inner account and
token outer account should be changed at the same time.
We can't ensure this, but we can instead have both circuits
verify that the vk of the other side is what it should be.
This is possible since every account update necessarily includes
the vk hash used as a precondition.
On the token outer account side, when a withdrawal action is only
valid if the token inner account update included in the Witness action
uses the correct vk hash,
and the same the other way around, that is,
a deposit is only valid if done with the expected vk hash.

This however creates a dangerous limbo state:
What if the token inner account is updated first,
but we do a deposit to the token outer account?
Those funds would be irreceivably lost.
The new circuit could accept both the new vk hash
and the one before that, if it's not a security vulnerability.

The same can be done the other way around when doing withdrawals.

## Governance (shared)

If the same governance controls both the rollup and the bridge zkapp,
then we want both the rollup and bridge to be updated at the same time.
Note that governance can not access the full transaction commitment,
since this is not made available to zkapps.
Instead, you can capture a subset by creating a helper token owner that creates
its own helper token account.
The helper token owner can have as children each of the outer accounts,
and those account updates can have as children the helper token account,
with permissions set to Parents_own_token at the first level and inherit
at the second.

## Withdrawal failsafe (unimplemented)

Consider the possibility that there is a bug in our token outer circuit,
allowing the user to withdraw all the deposited funds without doing a corresponding
withdrawal on the inside.
We do this by creating _two_ (or more, possibly) token outer accounts.
When you deposit your funds, you choose which one.
Half the time, only one of them is usable.
Specifically, half the time, the send permission will be Impossible,
and the vk will allow changing the vk (and permission) back during the other
portion of the time.
The other vk will implement similar logic, failing to work in the period when it
should be Impossible, transferring control back and resetting the permission.

Notably, you can still _deposit_ even when the send permission is Impossible.
You just can't withdraw from it.

Effectively, this means during a hack, if someone finds a vulnerability
in the withdrawal-allowing vk, it will only affect one of the accounts.
The other account is a separate circuit, and will need a separate vulnerability
to compromise.
It is much simpler, however, heavily reducing the surface for such bugs.

# Pseudo-code specs (unsynchronized, incorrect)

Rough OCaml-y specs follow.
You are meant to read the account update generating functions
as generating the account updates you are meant to use
to interact with the system.
Any account update not generated for the zkapps here is meant to be invalid.
Thus, you can infer the circuit behavior from this.
Anything that is impossible to check, e.g. the parent account update,
is simply not part of the circuit.
In certain places we use X instead of hash of X.
Hash of X is implied where it's not feasible to use X itself,
(e.g. storing the ledger in the app state of an account).

We don't fill out all the fields for account updates.
Assume an implicit `with default`.

## Core rollup spec

```ocaml
type ledger

type action

type action_state = action list

val macroslot_size : nat (* probably around 1 day *)

type macroslot = nat

type commit = { ledger : ledger ; inner_action_state ; action_state ; macroslot : nat }
type witness = { aux : field ; children : account_update_forest }

type outer_action =
  | Commit of commit
  | Bid of { sequencer : public_key ; rate : nat ; start : nat ; stop : nat ; valid_while : valid_while }
  | Witness of witness
  | Time of { valid_while : valid_while } (* specialization of Witness *)

type inner_action =
  | Witness of witness

type outer_app_state =
  { ledger : ledger
  ; inner_action_state : action_state
  ; sequencer : public_key
  ; macroslot : macroslot
  ; bid_rate : nat
  ; finalized : bool
  }

type inner_app_state =
  { outer_action_state : action_state
  }

let do_inner_step ~source_action_state ~actions =
  [ { public_key = inner_pk
    ; app_state =
      { outer_action_state = List.append actions source_action_state
      }
    ; preconditions =
      { outer_action_state = Some source_action_state
      }
    }
  ]

val max_valid_while_size : nat

val bid_min_time_before : nat (* probably 1 day *)
val bid_max_time_before : nat (* probably 2 days *)

val bid_deposit_factor : nat

val zeko_token_owner : account_id

let do_bid ~rate ~start ~stop ~valid_while =
  assert start - valid_while.upper > bid_min_time_before ;
  assert stop - valid_while.lower < bid_max_time_before ;
  [ { public_key = zeko_pk
    ; actions = Bid { rate ; start ; stop ; valid_while }
    ; preconditions = { valid_while }
    ; children =
      [ { account_id = zeko_token_owner
        ; public_key = zeko_pk
        ; 
        }
      ]
    }
  ]

let macroslot_upper macroslot = macroslot * (macroslot_size + 1)
let macroslot_lower macroslot = macroslot * macroslot_size

let get_winning_bid
  ~action_state_before_bid
  ~time_action
  ~actions_for_bid
  ~macroslot =
  (* bid logic *)
  let action_state = (actions_for_bid ++ (time_action :: action_state_before_bid)) in

  (* We only consider bids that have been made after some slot,
     time_action is an action before the bids begin that
     has a time bound before the bids are valid. *)
  (match time_action with
    | Witness _ -> assert false
    | Bid { valid_while ; _ } -> valid_while.upper < macroslot_lower macroslot - bid_max_time_before
    | Time { valid_while } -> valid_while.upper < macroslot_lower macroslot - bid_max_time_before
    | Commit { valid_while ; _ } -> valid_while.upper < macroslot_lower macroslot - bid_max_time_before
  )

  let Some (sequencer, rate) = List.fold actions_for_bid ~init:None ~f:(fun acc -> function
    | Bid { sequencer ; rate ; start ; stop ; _ } ->
      if start <= macroslot & stop > macroslot
        then (match acc with
          None -> (sequencer, rate)
          Some (sequencer', rate') ->
            if rate > rate'
              then (sequencer, rate)
              else (sequencer', rate')
        )
        else acc
    | _ -> acc
  ) in
  (action_state, sequencer, rate)


(* The case where we switch the sequencer peacefully,
   the previous sequencer has finalized, but we assume
   that it's stable, and continue working from that point.
*)
let do_switch_sequencer
  ~macroslot
  ~action_state_before_bid
  ~actions_for_bid
  ~time_action
  ~old_sequencer
  ~old_rate
  ~ledger
  =
  let (action_state, sequencer, rate) =
    get_winning_bid
      ~macroslot
      ~action_state_before_bid
      ~actions_for_bid
      ~time_action
      ~action_state
  in
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate = rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
          (* technically unneeded, but added for good measure *)
        ; macroslot = macroslot - 1
        ; sequencer = previous_sequencer
        ; bid_rate = old_rate
        ; finalized = true
        }
      ; valid_while =
        { lower = macroslot_lower (macroslot - 1)
          (* We allow using this in the previous macroslot too,
             since if they've finalized their turn we can begin
             ours early. *)
        ; upper = macroslot_upper macroslot
        }
      }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = sequencer
            ; amount = -rate * bid_deposit_factor
            }
          ; { token_id = zeko_token_owner
            ; public_key = zeko_kp
            ; amount =
              rate * bid_deposit_factor
              -
              old_rate * (bid_deposit_factor - 1)
            }
          ; { token_id = zeko_token_owner
            ; public_key = old_sequencer
            ; amount = old_rate * (bid_deposit_factor - 1)
            }
          ]
        }
      ]
    }
  ]

(* The case where we switch the sequencer,
   but the previous sequencer has not finalized their turn.
   NB: We must make sure that we are exactly in our macroslot,
   unlike the other cases.
*)
let do_switch_sequencer_penalize_unfinalized
  ~macroslot
  ~action_state_before_bid
  ~actions_for_bid
  ~time_action
  ~old_sequencer
  ~old_rate
  ~ledger
  =
  let (action_state, sequencer, rate) =
    get_winning_bid
      ~macroslot
      ~action_state_before_bid
      ~actions_for_bid
      ~time_action
      ~action_state
  in
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate = rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
          (* technically unneeded, but added for good measure *)
        ; macroslot = macroslot - 1
        ; sequencer = previous_sequencer
        ; bid_rate = old_rate
        ; finalized = false
        }
      ; valid_while
      ; action_state
      }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = sequencer
            ; amount = -rate * bid_deposit_factor
            }
          ; { token_id = zeko_token_owner
            ; public_key = zeko_kp
            ; amount =
              rate * bid_deposit_factor
            }
          ]
        }
      ]
    }
  ]

(* The case where we switch the sequencer,
   but the previous sequencer has not finalized more than once.
*)
let do_switch_sequencer_penalize_double_finalized
  ~macroslot
  ~action_state_before_bid
  ~actions_for_bid
  ~time_action
  ~old_sequencer
  ~old_rate
  ~ledger
  ~signature
  ~other_ledger
  =
  assert verify_signature ~signature ~public_key:old_sequencer ~data:(other_ledger, macroslot - 1)
  let (action_state, sequencer, rate) =
    get_winning_bid
      ~macroslot
      ~action_state_before_bid
      ~actions_for_bid
      ~time_action
      ~action_state
  in
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate = rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
          (* technically unneeded, but added for good measure *)
        ; macroslot = macroslot - 1
        ; sequencer = previous_sequencer
        ; bid_rate = old_rate
        ; finalized = true
        }
      ; valid_while
      ; action_state
      }
    ; children =
      [ { account_id = zeko_token_owner
        ; children =
          [ { token_id = zeko_token_owner
            ; public_key = sequencer
            ; amount = -rate * bid_deposit_factor
            }
          ; { token_id = zeko_token_owner
            ; public_key = zeko_kp
            ; amount =
              rate * bid_deposit_factor
            }
          ]
        }
      ]
    }
  ]

(* Commit work to L1. *)
let do_commit
  ~txn_snark
  ~macroslot
  ~valid_while
  ~new_actions
  =
  assert valid_while.upper - valid_while.lower <= max_valid_while_size ;
  assert valid_while.lower >= macroslot_lower (macroslot - 1)
    (* The sequencer can start committing in the macroslot before,
       provided that the previous sequencer has finalized *) ;
  assert valid_while.upper < macroslot_upper macroslot ;

  let old_inner = get_account inner_pk txn_snark.source in
  let new_inner = get_account inner_pk txn_snark.target in

  let action_state =
    (* We don't force sequencer to match on latest action state,
       since it's unreliable and might roll back. *)
    List.append new_actions new_inner.app_state.outer_action_state in

  let ledger = txn_snark.target in
  let inner_action_state = new_inner.action_state in

  [ { public_key = zeko_pk
    ; actions = Commit { ledger ; inner_action_state ; ~valid_while }
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate
      ; finalized = false
      }
    ; preconditions =
      { app_state =
        { ledger = Some txn_snark.source
        ; inner_action_state = Some old_inner.action_state
          (* technically unneeded, but added for good measure *)
        ; macroslot
        ; sequencer
        ; bid_rate
        ; finalized = false
        }
      ; valid_while
      ; action_state
      }
    }
  ]

(* End our turn *)
let do_finalize_macroslot
  ~sequencer
  ~macroslot
  ~bid_rate
  ~ledger
  ~signature
  =
  assert verify_signature ~signature ~public_key:sequencer ~data:(ledger, macroslot)
  let inner_action_state = (get_account inner_pk ledger).action_state in
  [ { public_key = zeko_pk
    ; events = signature
    ; app_state =
      { ledger
      ; inner_action_state
      ; macroslot
      ; sequencer
      ; bid_rate = rate
      ; finalized = true
      }
    ; preconditions =
      { app_state =
        { ledger
        ; inner_action_state
        ; macroslot
        ; sequencer
        ; bid_rate
        ; finalized = false
        }
      ; valid_while =
        { lower = macroslot_lower macroslot
        ; upper = macroslot_upper macroslot
        }
      }
    }
  ]

let do_time ~valid_while =
  [ { public_key = zeko_pk
    ; actions = [ Time { valid_while } ]
    ; preconditions =
      { valid_while = valid_while
      }
    }
  ]

let do_witness_outer ~aux ~children =
  [ { public_key = zeko_pk
    ; actions = [ Witness { aux ; children } ]
    ; preconditions =
      { children
      }
    }
  ]

let do_witness_inner ~aux ~children =
  [ { public_key = inner_pk
    ; actions = [ Witness { aux ; children } ]
    ; preconditions =
      { children
      }
    }
  ]

```

## Pseudo-code example spec for token transfer smart contract

TODO: use fungible token standard contract

```ocaml
val public_key : PublicKey.t
val token_id : TokenId.t

type deposit = { amount : nat ; recipient : PublicKey.t ; timeout : slot }
type withdrawal = { amount : nat ; recipient : PublicKey.t }

type token_outer_helper_state =
  { next_withdrawal : nat
  ; next_cancelled_deposit : nat
  }

type token_inner_helper_state =
  { next_deposit : nat
  }

let deposit_action ~(sender : PublicKey.t) (deposit : deposit) : outer_action =
  let children =
    [ { account_id = token_id (* token owner *)
      ; children =
        [ { public_key = sender
          ; token_id
          ; balance_change = -deposit.amount
          ; may_use_token = Parents_own_token
          }
        ; { public_key
          ; token_id
          ; balance_change = deposit.amount
          ; may_use_token = Parents_own_token
          }
        ]
      }
    ]
  in
  Match { data = deposit ; children }


let do_deposit ~sender (deposit : deposit) =
  [ { public_key = zeko_pk
    ; actions = [ deposit_action ~sender deposit ]
    ; children
    }
  ]

let withdraw_action ~(sender : PublicKey.t) (withdrawal : withdrawal) : inner_action =
  let children =
    [ { public_key
      ; children =
        [ { public_key = sender
          ; token_id = AccountId.create public_key TokenId.default
          ; balance_change = -withdrawal.amount
          ; may_use_token = Parents_own_token
          }
      }
    ]
  in
  Match { data = withdrawal ; children }

let do_withdrawal ~sender (withdrawal : withdrawal) =
  [ { public_key = inner_pk
    ; actions = [ withdraw_action ~sender withdrawal ]
    ; children
    }
  ]

let do_finalize_deposit ~sender ~deposit ~actions_after_deposit ~action_state_before_deposit ~prev_next_deposit =
  assert check_accepted ~deposit ~actions_after_deposit = `Accepted ;
  assert prev_next_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    (deposit_action ~sender deposit :: actions_after_deposit)
    ++ action_state_before_deposit
  in
  [ { public_key
    ; children =
      [ { public_key = deposit.recipient
        ; token_id = AccountId.create public_key TokenId.default
        ; authorization_kind = Signature
        ; use_full_commitment = true
        ; balance_change = deposit.amount
        ; app_state =
          { next_deposit = action_state_before_deposit.length + 1
          }
        ; preconditions =
          { app_state =
            { next_deposit = prev_next_deposit
            }
          }
        }
      ; { public_key = inner_pk
        ; preconditions = { app_state = { outer_action_state } }
        }
      ]
    }
  ]

let do_finalize_cancelled_deposit ~sender ~deposit ~actions_after_deposit ~action_state_before_deposit ~prev_next_cancelled_deposit =
  assert check_accepted ~deposit ~actions_after_deposit = `Rejected ;
  assert prev_next_cancelled_deposit <= action_state_before_deposit.length ;
  let outer_action_state =
    (deposit_action ~sender deposit :: actions_after_deposit)
    ++ action_state_before_deposit
  in
  [ { account_id = token_id
    ; children =
      [ { public_key
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = -deposit.amount
        ; children =
          [ { public_key = zeko_pk
            ; preconditions = { action_state = outer_action_state }
            }
          ; { public_key = deposit.recipient
            ; token_id = Account_id.create public_key token_id
            ; may_use_token = Parents_own_token
            ; use_full_commitment = true
            ; authorization_kind = Signature
            ; app_state =
              { next_cancelled_deposit = action_state_before_deposit.length + 1
              }
            ; preconditions =
              { app_state =
                { next_cancelled_deposit = Some prev_next_cancelled_deposit
                }
              }
            }
          ]
        }
      ; { public_key = deposit.recipient
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = deposit.amount
        }
      ]
    }
  ]

val withdrawal_delay : nat

let do_finalize_withdrawal ~sender ~withdrawal ~actions_after_withdrawal ~action_state_before_withdrawal ~actions_after_commit ~action_state_before_commit ~commit ~prev_next_withdrawal =
  assert prev_next_withdrawal <= action_state_before_withdrawal.length ;
  let inner_action_state =
    actions_after_withdrawal
    ++ withdraw_action ~sender withdrawal :: action_state_before_withdrawal
  in
  let outer_action_state =
    actions_after_commit
    ++ Commit commit :: action_state_before_commit
  in
  [ { account_id = token_id
    ; children =
      [ { public_key
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = -withdrawal.amount
        ; children =
          [ { public_key = zeko_pk
            ; preconditions =
              { action_state = outer_action_state
              ; app_state = { inner_action_state }
              ; valid_while =
                { lower = commit.valid_while.upper + withdrawal_delay
                ; upper = infinity }
              }
            }
          ; { public_key = withdrawal.recipient
            ; token_id = Account_id.create public_key token_id
            ; may_use_token = Parents_own_token
            ; use_full_commitment = true
            ; authorization_kind = Signature
            ; app_state =
              { next_withdrawal = action_state_before_withdrawal.length + 1
              }
            ; preconditions =
              { app_state =
                { next_withdrawal = Some prev_next_withdrawal
                }
              }
            }
          ]
        }
      ; { public_key = withdrawal.recipient
        ; token_id
        ; may_use_token = Parents_own_token
        ; balance_change = withdrawal.amount
        }
      ]
    }
  ]
```
