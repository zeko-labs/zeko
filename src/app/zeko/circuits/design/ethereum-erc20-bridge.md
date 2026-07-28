# Ethereum ERC-20 to Zeko fungible-token bridge

Status: implementation design and gap analysis, 2026-07-21.

This note defines the L2 Mina fungible-token topology for an ERC-20 bridge and
connects it to the existing OCaml bridge circuits. It is intentionally narrower
than the general bridge specification: Ethereum owns ERC-20 custody and timeout
refunds, while Zeko proves the accepted rollup transition and the L2 custom-token
movement.

## Decision

Use the unmodified Mina Foundation `FungibleToken` contract as the L2 token
owner, a separate admin contract, and a proof-controlled bridge-vault account
under that token. For the first implementation, pre-mint a bounded inventory to
the bridge vault and use balanced vault-to-user/user-to-vault transfers.

This is the supply model that the current OCaml custom-token rules can be
adapted to. Exact mint-on-deposit and burn-on-withdrawal are supported by the
Mina fungible-token standard in principle, but are **not** what
`Bridge_rules.Make_custom` currently proves. They require a different bridge
circuit and an admin policy that authorizes each mint from the accepted Ethereum
deposit proof.

The Ethereum side of a deposit remains the existing witness-action path:
`submitDeposit` transfers ERC-20 into Solidity custody and emits the canonical
deposit fields; those fields become the proof witness action consumed by
`Check_accepted`, just as a native Mina deposit's account-update forest becomes
the witness action. The current Ethereum branch already replaces Mina children
with a salted hash of the deposit parameters
([`bridge_state.ml`, lines 307-325](../bridge_state.ml#L307-L325)). The ERC-20
version must extend that preimage with an asset identity.

## Why the standard token contract fits

Mina's standard deliberately keeps custom logic out of the token owner. Token
balances are ledger accounts under a token ID derived from the owner, complex
transactions are approved as account-update forests, and privileged minting is
delegated to a separate admin contract. This separation is part of
[RFC 14](https://github.com/o1-labs/rfcs/blob/main/0014-fungible-token-standard.md),
not an application convention.

The maintained reference implementation has the properties this bridge needs:

- `TokenContract.deriveTokenId()` derives the managed token from the owner's
  account, and a token contract can approve a forest containing at most nine
  token-using account updates
  ([o1js `TokenContract`](https://github.com/o1-labs/o1js/blob/main/src/lib/mina/v1/token/token-contract.ts#L48-L159)).
- `FungibleToken.approveBase` requires the token balance changes to sum to zero,
  rejects a positive running balance (tokens cannot be received before they are
  sent), and prevents changes to token-account `access` and `receive`
  permissions
  ([reference implementation](https://github.com/MinaFoundation/mina-fungible-token/blob/main/FungibleToken.ts)).
- `mint` asks the configured admin contract for authorization; `burn` reduces a
  holder balance; both update the reserved circulation account
  ([reference implementation, mint and burn](https://github.com/MinaFoundation/mina-fungible-token/blob/main/FungibleToken.ts#L143-L170)).
- The owner stores `decimals` as `UInt8`, while token movements use `UInt64`
  ([standard API](https://minafoundation.github.io/mina-fungible-token/api.html)).
- Deployment creates a reserved circulation account at the token owner's public
  key under the derived token ID. The official deployment guide recommends
  deploying the admin, token, and initialization atomically and funding all new
  accounts in the same transaction
  ([deployment guide](https://minafoundation.github.io/mina-fungible-token/deploy.html)).

Do not fork `FungibleToken` to add bridge behavior. The standard's
interoperability benefit comes from using the same owner implementation. For
the bounded-inventory model, the admin is used only during deployment;
bridge-specific runtime policy belongs in the bridge and bridge-vault
verification key.

## L2 account topology

For every registered ERC-20 asset:

| Role | Mina account ID | Authorization and purpose |
| --- | --- | --- |
| Token owner | `(ft_owner_pk, TokenId.default)` | Unmodified `FungibleToken`; owns `ft_token_id = derive_token_id(owner_account_id)` and runs `approveBase`. |
| Token admin | `(ft_admin_pk, TokenId.default)` | Unmodified separate admin contract. It authorizes the bounded deployment mint, then its controller is permanently set to `Public_key.Compressed.empty`; it provides no post-registration administration. |
| Circulation account | `(ft_owner_pk, ft_token_id)` | Reserved by the standard for supply accounting. Never use it as the bridge vault. |
| Bridge vault | `(shared_vault_l2, ft_token_id)` | Holds that asset's pre-minted inventory and uses the universal bridge verification key. Every asset shares the public key and VK but has a distinct derived token ID/account/balance. |
| User balance | `(user_pk, ft_token_id)` | Ordinary standard fungible-token account. |
| Deposit replay helper | `(user_pk, derive_token_id(bridge_vault_account_id))` | Stores `next_deposit`; it is a subtoken of the bridge vault, not an FT balance. |
| Rollup inner account | `(zeko_l2, TokenId.default)` | Existing rollup state and synchronized Ethereum action checkpoint. |

`token_owner_l2` in the circuit must be the token owner's **account ID**, not
the derived token ID. The existing helper `token_owner_id` performs that
derivation for custom assets
([`zeko_util.ml`, lines 433-437](../zeko_util.ml#L433-L437)).

Each supported ERC-20 gets its own token owner, derived token ID, inventory,
fee policy, and immutable registry entry. The owner and token ID are dynamic
verified-record values. All assets use the same registry-configured vault public
key and universal bridge circuit/verification key; the derived token ID keeps
their vault balances and replay helpers independent.

## Asset and amount binding

A multi-asset bridge cannot identify an asset only through an off-chain archive
record. Define a versioned registry record and bind it into both directions:

```text
asset_domain_v1 = keccak256(bytes("ZEKO_ERC20_ASSET_V1"))
asset_id_v1 = keccak256(abi.encode(
  asset_domain_v1,
  ethereum_chain_id,
  solidity_bridge_address,
  erc20_address,
  canonical_mina_token_owner_account_id,
  ft_token_id,
  decimals
))
```

Represent the 256-bit ID in proof fields as two unsigned 128-bit limbs. Do not
reduce a `bytes32` modulo the Mina field, which would introduce aliases. The
registry and every deposit/withdrawal preimage must bind the version, both ID
limbs, ERC-20 address, L2 owner/token ID, and decimals. Solidity, the OCaml
action constructor, settlement guest, SDK, and indexer must implement one
canonical encoding and reject unknown versions.

Use identical base units on both chains:

- require the registered ERC-20 decimals to equal the L2 token's `UInt8`
  decimals;
- require every bridged amount to fit `UInt64` before Solidity emits an action;
- perform no decimal scaling or rounding; and
- initially reject fee-on-transfer, rebasing, callback-bearing, or otherwise
  non-exact ERC-20s. Solidity must verify that custody increased by exactly the
  submitted gross amount.

Split fees by denomination. An ERC-20 fee is an asset amount enforced by
Solidity. Mina transaction/proving and account-creation fees are MINA amounts
funded by a default-token fee payer or sponsor. Never subtract a Mina account
creation fee from a custom-token `UInt64` amount.

## Deposit flow: Ethereum to Zeko

1. `submitDeposit(token, amount, recipient)` resolves the token's immutable
   registry entry, transfers exactly `amount` base units into Solidity custody,
   fixes the no-cancellation timeout to `UInt32.max`, and emits the canonical
   `BridgeDeposit` witness fields plus `ERC20DepositSubmitted` with the asset ID.
2. The Ethereum action importer converts the canonical deposit log and immutable
   registry identity into the same versioned
   `Outer_action.Witness` preimage on every node. The witness contains no Mina L1
   account-update children; ERC-20 custody is proven by the settlement/Ethereum
   domain binding.
3. `Check_accepted` follows the existing rule: the deposit is accepted only
   after a synchronized commit includes it before its timeout, or rejected after
   the timeout path
   ([`check_accepted_make.ml`, lines 58-125](../check_accepted_make.ml#L58-L125)).
4. Finalization on L2 proves the accepted deposit, advances the recipient's
   replay helper, debits the custom-token bridge vault, and credits the
   recipient. The current rule already binds the accepted proof, derives the
   helper token, requires a strictly increasing deposit index, and emits the
   deposit event
   ([`rule_bridge_finalize_deposit.ml`, lines 118-255](../rule_bridge_finalize_deposit.ml#L118-L255)).
5. The entire custom-token subtree is passed to the standard owner's
   `approveBase`; its token changes must be conserved and ordered debit-first.

The valid L2 forest is conceptually:

```text
FungibleToken.approveBase                         (owner, default token)
└── bridge vault: -net_amount, Proof             (ft_token_id, Parents_own_token)
    ├── replay helper update, user signature     (vault subtoken, Parents_own_token)
    ├── rollup inner-state witness               (default token, No)
    └── recipient: +net_amount, no auth          (ft_token_id, Inherit_from_parent)
```

If an L2 asset fee is desired, add a custom-token fee recipient after the vault
debit and keep the subtree sum zero. The simpler first release charges the
ERC-20 fee in Solidity and makes the OCaml `amount` the net amount, so the L2
subtree is a one-for-one vault transfer.

New custom-token accounts must set `implicit_account_creation_fee = false` and
be funded from MINA fee excess. Mina transaction logic explicitly forbids an
implicit creation fee on a non-default token
([`zkapp_command_logic.ml`, lines 1426-1465](../../../../lib/transaction_logic/zkapp_command_logic.ml#L1426-L1465)).

## Withdrawal flow: Zeko to Ethereum

1. The user constructs a custom-token forest approved by the standard owner.
   The sender debit must occur before the bridge-vault credit; the reference
   `approveBase` rejects a positive prefix as flash minting.
2. The bridge-vault receive update uses the existing `inner_receive` proof. Its
   purpose is to satisfy the vault's proof access permission while binding the
   amount and token ID
   ([`rule_bridge_inner_receive.ml`, lines 18-40](../rule_bridge_inner_receive.ml#L18-L40)).
3. The rollup inner witness action binds asset ID, amount, Ethereum recipient,
   and the exact approved child forest. A separate default-MINA debit/payout may
   fund proving; it must sit outside the custom-token conservation subtree.
4. After the existing commit and withdrawal-delay checks, settlement exports
   the exact action and Solidity releases the registered ERC-20 from custody to
   the bound recipient. The OCaml withdrawal proof already binds the withdrawal
   action to the inner action-state extension and a committed outer action
   ([`rule_bridge_finalize_withdrawal.ml`, lines 128-168](../rule_bridge_finalize_withdrawal.ml#L128-L168));
   Ethereum replaces the Mina L1 payout portion with Solidity settlement.

The required submission subtree is:

```text
FungibleToken.approveBase                         (owner, default token)
├── user: -amount, Signature                     (ft_token_id, Parents_own_token)
└── bridge vault: +amount, bridge Proof          (ft_token_id, Parents_own_token)

rollup witness / MINA fee updates                 (default-token siblings)
```

On timeout cancellation, Solidity refunds the original ERC-20 custody entry.
The Mina L1 custom-token cancellation circuit is not part of the Ethereum
path.

## Vault inventory versus mint/burn

Both models are compatible with the **standard**, but only one is compatible
with the current bridge circuits.

### Bounded vault inventory (selected first release)

During deployment the admin mints a fixed bridge capacity to the bridge vault.
Deposits move inventory vault-to-user; withdrawals return it user-to-vault. The
standard owner's `approveBase` sees a balanced forest, matching the balance
changes generated today by `finalize_deposit`, `withdrawal_action`, and
`inner_receive`.

Required invariants:

- `user_wrapped_supply <= ethereum_locked_for_asset` at every accepted state;
- Solidity rejects deposits whose outstanding per-token liability would exceed
  an immutable `depositCapByToken`, configured to equal the initial pre-minted
  L2 vault inventory;
- a deposit cannot finalize if the vault lacks the net amount;
- the bounded mint is auditable and the final standard admin controller is the
  fixed non-signing `Public_key.Compressed.empty`;
- no path can transfer from the reserved circulation account; and
- rollup commit freezing and the Ethereum bridge pause stop cross-chain
  progress; ordinary L2 standard-token transfers remain enabled.

The tradeoff is supply reporting: the standard circulation account counts the
pre-minted vault inventory even while it is idle, so `getCirculating()` is not
the economic amount backed by Ethereum custody. Indexers must expose both
standard minted supply and `minted_supply - vault_balance`. The capacity also
places an explicit upper bound on outstanding wrapped ERC-20.

### Exact mint on deposit and burn on withdrawal (future design)

The standard exposes `mint` and `burn`, so this model does not require a fork of
`FungibleToken`. It does require new OCaml/account-update semantics:

- accepted-deposit finalization must call standard `mint`, and a bridge-aware
  admin must authorize exactly `(asset_id, deposit_index, recipient, amount)`;
- withdrawal submission must call standard `burn` and bind that burn to the
  emitted rollup witness action; and
- replay state, pause state, admin authorization, and action ordering must be
  proven in those new methods.

An unbalanced child forest sent to `approveBase` is not minting and will be
rejected. Therefore exact mint/burn cannot be enabled by changing a balance,
`authorization_kind`, or deployment flag in `Make_custom`; it is a separate
circuit/admin design. Its advantage is exact standard circulation accounting
and no vault-capacity ceiling.

## Connection to the existing OCaml functors

`Make_mina` fixes both sides to the default token and base parameters, while
`Make_custom` fixes **both** L1 and L2 to custom-token owners and custom
parameters
([`bridge_rules.ml`, lines 5-69 and 132-200](../bridge_rules.ml#L5-L200)). An
Ethereum ERC-20 bridge is a hybrid:

```text
L1 custody/action source: Ethereum ERC-20, no Mina token owner
L1 deposit preimage:       versioned Ethereum/ERC-20 parameters
L2 asset:                  custom Mina FungibleToken
L2 withdrawal forest:      custom parameters and token-owner approval
```

Introduce a side-specific asset functor (or `Make_ethereum_custom`) instead of
reusing `Make_custom` unchanged. In particular, the current Ethereum action
hashes whichever `Deposit_params` module the enclosing functor selected. Using
`Make_custom` would make Solidity bind irrelevant Mina authorization/call-forest
fields and still would not provide a canonical ERC-20 identity
([`bridge_state.ml`, lines 230-325](../bridge_state.ml#L230-L325)).

### Required circuit corrections

The existing custom path compiles, but its generated forests are not yet valid
standard-token transactions:

1. `withdrawal_action` prepends the positive vault receive before caller-supplied
   nested children
   ([`bridge_state.ml`, lines 379-402](../bridge_state.ml#L379-L402)). A standard
   sender debit in `nested_children` therefore comes too late and triggers the
   reference implementation's flash-mint check. Permit a constrained debit-first
   ordering.
2. Deposit-finalization custom-token payouts are children of the vault update
   but use `Parents_own_token`
   ([`rule_bridge_finalize_deposit.ml`, lines 245-306](../rule_bridge_finalize_deposit.ml#L245-L306)).
   A direct child's `Parents_own_token` selects the token derived from the vault
   account; it does not retain `ft_token_id`. Those payouts must use
   `Inherit_from_parent`, while the replay helper intentionally keeps
   `Parents_own_token`. The caller derivation and non-default-token check are in
   [`zkapp_command_logic.ml`, lines 1043-1108 and 1224-1236](../../../../lib/transaction_logic/zkapp_command_logic.ml#L1043-L1108).
3. The custom payouts set `implicit_account_creation_fee = true` and subtract a
   MINA creation fee from the custom-token amount
   ([`rule_bridge_finalize_deposit.ml`, lines 257-297](../rule_bridge_finalize_deposit.ml#L257-L297)).
   Non-default tokens cannot pay this implicit fee, and the subtraction also
   makes the custom-token forest unbalanced. Use a separate MINA funding update
   and denomination-specific fees.

Equivalent corrections are needed in any retained custom-Mina L1
finalize/cancel rules. For the Ethereum bridge, Solidity handles those L1 paths.

## Implemented runtime seam

This branch implements the security-critical cross-chain seam:

- `Make_ethereum_token` combines an Ethereum deposit source with a custom-token
  L2 vault and fixes custom-token payout denomination, token inheritance, and
  debit-first withdrawal ordering;
- the settlement bridge guest converts a canonical Solidity `submitDeposit`
  log into the exact asset-bound outer Witness action consumed by
  `Check_accepted`;
- the sequencer archive/export and settlement guest support an asset-bound
  token-withdrawal preimage and V3 claim leaf; and
- Solidity, the gateway, and the browser SDK implement canonical ERC-20
  custody, indexing, delayed claim, per-asset replay protection, and liability
  accounting.

The executable Mina Fungible Token orchestration follows the same boundary:

- the sequencer config accepts the registry account, schema, approved MFT
  standard VK ID, shared vault public key, and universal bridge VK ID;
- public bridge types, prover jobs, VK responses, and proof dispatch carry a
  verified asset record plus membership path through one
  `Make_ethereum_assets` circuit family;
- registration uses the existing recursive folder pattern to scan every dense
  old leaf at the next exact index, reject duplicate Ethereum token, asset ID,
  owner, or derived token ID, verify an empty append slot, and commit the
  incremented root/count;
- GraphQL exposes proof-only token deposit-finalization and withdrawal-request
  mutations plus the immutable public token configuration;
- withdrawal input carries the complete standard token-owner account-update
  body. The circuit pins its public key and default token ID while the standard
  owner's proof enforces all remaining `approveBase` semantics;
- the bridge SDK checks the returned vault forest and owner public input, grafts
  the genuine standard-token proof/signature authorizations, and only then
  submits the complete L2 transaction; and
- the operator deployment helper validates each pending Solidity record and
  Zeko membership, deploys unmodified `FungibleToken`/`FungibleTokenAdmin`,
  locks each token-specific shared-vault account to the universal VK with
  proof-authorized sends, and mints exactly the registered cap.

The archive and Actions indexer reconstruct immutable records and refreshed
membership paths after later appends. They are availability aids, not security
boundaries: the circuit authenticates the registry account root/count, and the
settlement guest binds the OCaml Poseidon transition to the same ordered
canonical records activated by Solidity.


## Deployment and post-registration policy

Onboard one asset atomically where possible:

1. propose the exact immutable Solidity record in `Pending` state;
2. deploy the admin and unmodified `FungibleToken`, initialize decimals and
   circulation, and verify the expected standard verification keys;
3. create the token-specific account at the shared vault public key with the
   universal bridge VK and locked proof permissions;
4. mint the bounded inventory to the vault, then rotate the unmodified standard
   admin controller to the fixed non-signing `Public_key.Compressed.empty`;
5. fund all default-token account-creation costs and helper sponsorship policy;
6. append the canonical record through the exhaustive Zeko registry transition,
   settle the new root/count and ordered record batch, and activate exactly that
   pending Solidity proposal; and
7. enable deposits only after a cross-chain deposit/withdrawal rehearsal.

Set the standard token owner's `allowUpdates` to false. Registration permanently
revokes the standard admin controller, so token-level `pause`, `resume`,
`setAdmin`, and authorized verification-key upgrades are unavailable afterward.
The supported emergency controls are rollup commit freezing and the Ethereum
bridge pause; neither disables ordinary L2 standard-token transfers. A future
token-owner, admin, vault, circuit, settlement, or Solidity registry change
requires a separately reviewed cross-chain migration rather than the revoked
standard-admin path.

## Verification coverage

The branch now has these executable gates:

1. OCaml, Rust, Solidity, and TypeScript share canonical record, asset/action,
   registry-transition, and settlement receipt encodings.
2. Registry vectors cover empty, populated, and maximum-size scans plus wrong
   roots/counts, repeated/skipped/reordered leaves, non-empty append slots, and
   every uniqueness dimension.
3. Universal circuit vectors exercise two dynamic owners through one circuit
   tag/VK, registry membership rejection, accepted deposit finalization,
   helper-account progression, denomination, token inheritance, and withdrawal
   action/export fields.
4. The bridge SDK rejects malformed/wrong-asset/wrong-owner/wrong-VK forests and
   runs a local ledger roundtrip through the unmodified standard owner's
   `approveBase`: exact-cap vault provisioning, vault-to-user deposit, then
   user-to-vault withdrawal.
5. Settlement guest, gateway, and Solidity tests cover V4 batched registry
   synchronization, pending/activation status, V2 deposits, V3 withdrawals,
   per-token caps/liabilities, replay rejection, and delayed release.
6. The full local gate uses the real sequencer/prover, three DA nodes, Actions
   services, two unmodified token owners, one shared vault key/VK, Anvil custody,
   and exact claims without generating an SP1 proof.

After implementation, run the repository gates from a persistent `tmux` session
for the heavy commands:

```bash
dune build ./src/app/zeko/sequencer ./src/app/zeko/da_layer ./src/app/zeko/signer
dune exec ./src/app/zeko/tests/ethereum_bridge_vectors.exe
dune exec ./src/app/zeko/tests/compile_circuits.exe
dune exec ./src/app/zeko/tests/test_all_real.exe
src/app/zeko/sequencer/tests/run-sequencer-test.sh real 1 false true
```
