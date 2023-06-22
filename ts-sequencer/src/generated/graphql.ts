/* eslint-disable */
import { GraphQLResolveInfo, GraphQLScalarType, GraphQLScalarTypeConfig } from 'graphql';
export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
export type MakeEmpty<T extends { [key: string]: unknown }, K extends keyof T> = { [_ in K]?: never };
export type Incremental<T> = T | { [P in keyof T]?: P extends ' $fragmentName' | '__typename' ? T[P] : never };
export type RequireFields<T, K extends keyof T> = Omit<T, K> & { [P in K]-?: NonNullable<T[P]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: { input: string | number; output: string; }
  String: { input: string; output: string; }
  Boolean: { input: boolean; output: boolean; }
  Int: { input: number; output: number; }
  Float: { input: number; output: number; }
  /** account nonce */
  AccountNonce: { input: any; output: any; }
  /** action */
  Action: { input: any; output: any; }
  /** amount */
  Amount: { input: any; output: any; }
  /** Kind of authorization required */
  AuthRequired: { input: any; output: any; }
  /** balance */
  Balance: { input: any; output: any; }
  BlockTime: { input: any; output: any; }
  /** A reference to how the block header refers to the body of the block as a hex-encoded string */
  BodyReference: { input: any; output: any; }
  /** Base58Check-encoded chain hash */
  ChainHash: { input: any; output: any; }
  CurrencyAmount: { input: any; output: any; }
  /** epoch */
  Epoch: { input: any; output: any; }
  /** Base58Check-encoded epoch seed */
  EpochSeed: { input: any; output: any; }
  /** Block encoded in extensional block format */
  ExtensionalBlock: { input: any; output: any; }
  /** fee */
  Fee: { input: any; output: any; }
  /** fee transfer type */
  FeeTransferType: { input: any; output: any; }
  /** String representing an Fp Field element */
  Field: { input: any; output: any; }
  /** field element */
  FieldElem: { input: any; output: any; }
  GlobalSlot: { input: any; output: any; }
  /** globalslot */
  Globalslot: { input: any; output: any; }
  /** ocaml integer as a string */
  Index: { input: any; output: any; }
  /** network address */
  InetAddr: { input: any; output: any; }
  /** Arbitrary JSON */
  JSON: { input: any; output: any; }
  /** Base58Check-encoded ledger hash */
  LedgerHash: { input: any; output: any; }
  /** length */
  Length: { input: any; output: any; }
  Memo: { input: any; output: any; }
  /** Base58Check-encoded hash of a pending coinbase auxiliary hash */
  PendingCoinbaseAuxHash: { input: any; output: any; }
  /** Base58Check-encoded hash of a pending coinbase hash */
  PendingCoinbaseHash: { input: any; output: any; }
  /** Block encoded in precomputed block format */
  PrecomputedBlock: { input: any; output: any; }
  /** Base-64 encoded proof */
  PrecomputedBlockProof: { input: any; output: any; }
  /** Base58Check-encoded private key */
  PrivateKey: { input: any; output: any; }
  /** Base58Check-encoded public key string */
  PublicKey: { input: any; output: any; }
  /** A transaction encoded in the Rosetta format */
  RosettaTransaction: { input: any; output: any; }
  /** zkApp command for a test zkApp */
  SendTestZkappInput: { input: any; output: any; }
  Sign: { input: any; output: any; }
  Signature: { input: any; output: any; }
  /** slot */
  Slot: { input: any; output: any; }
  /** span */
  Span: { input: any; output: any; }
  /** Base58Check-encoded hash of the staged ledger hash's aux_hash */
  StagedLedgerAuxHash: { input: any; output: any; }
  /** Base58Check-encoded state hash */
  StateHash: { input: any; output: any; }
  /** Experimental: Bigint field-element representation of stateHash */
  StateHashAsDecimal: { input: any; output: any; }
  Time: { input: any; output: any; }
  /** String representation of a token's UInt64 identifier */
  TokenId: { input: any; output: any; }
  /** Base58Check-encoded transaction hash */
  TransactionHash: { input: any; output: any; }
  /** Base64-encoded transaction */
  TransactionId: { input: any; output: any; }
  /** transaction status failure */
  TransactionStatusFailure: { input: any; output: any; }
  /** String representing a uint32 number in base 10 */
  UInt32: { input: any; output: any; }
  /** String or Integer representation of a uint64 number. If the input is a string, it must represent the number in base 10 */
  UInt64: { input: any; output: any; }
  /** The kind of user command */
  UserCommandKind: { input: any; output: any; }
  /** verification key in Base64 format */
  VerificationKey: { input: any; output: any; }
  /** Hash of verification key */
  VerificationKeyHash: { input: any; output: any; }
  /** truncated vrf output */
  VrfOutputTruncated: { input: any; output: any; }
  /** consensus vrf scalar */
  VrfScalar: { input: any; output: any; }
  ZkappProof: { input: any; output: any; }
};

/** An account record according to the daemon */
export type Account = {
  __typename?: 'Account';
  /** Action state associated with this account */
  actionState?: Maybe<Array<Scalars['Action']['output']>>;
  /** The amount of MINA owned by the account */
  balance: AnnotatedBalance;
  /**
   * The public key to which you are delegating - if you are not delegating to anybody, this would return your public key
   * @deprecated use delegateAccount instead
   */
  delegate?: Maybe<Scalars['PublicKey']['output']>;
  /** The account to which you are delegating - if you are not delegating to anybody, this would return your public key */
  delegateAccount?: Maybe<Account>;
  /**
   * The list of accounts which are delegating to you (note that the info is
   * recorded in the last epoch so it might not be up to date with the current
   * account status)
   */
  delegators?: Maybe<Array<Account>>;
  /** The account that you delegated on the staking ledger of the current block's epoch */
  epochDelegateAccount?: Maybe<Account>;
  /** The index of this account in the ledger, or null if this account does not yet have a known position in the best tip ledger */
  index?: Maybe<Scalars['Int']['output']>;
  /**
   * Like the `nonce` field, except it includes the scheduled transactions
   * (transactions not yet included in a block) (stringified uint32)
   */
  inferredNonce?: Maybe<Scalars['AccountNonce']['output']>;
  /**
   * The list of accounts which are delegating to you in the last epoch (note that
   * the info is recorded in the one before last epoch epoch so it might not be up
   * to date with the current account status)
   */
  lastEpochDelegators?: Maybe<Array<Account>>;
  /** The base58Check-encoded hash of this account to bootstrap the merklePath */
  leafHash?: Maybe<Scalars['FieldElem']['output']>;
  /** True if locked, false if unlocked, null if the account isn't tracked by the queried daemon */
  locked?: Maybe<Scalars['Boolean']['output']>;
  /** Merkle path is a list of path elements that are either the left or right hashes up to the root */
  merklePath?: Maybe<Array<MerklePathElement>>;
  /** A natural number that increases with each transaction (stringified uint32) */
  nonce?: Maybe<Scalars['AccountNonce']['output']>;
  /** Permissions for updating certain fields of this account */
  permissions?: Maybe<AccountPermissions>;
  /** Path of the private key file for this account */
  privateKeyPath: Scalars['String']['output'];
  /** Boolean indicating whether all 8 fields on zkAppState were last set by a proof-authorized account update */
  provedState?: Maybe<Scalars['Boolean']['output']>;
  /** The public identity of the account */
  publicKey: Scalars['PublicKey']['output'];
  /** Top hash of the receipt chain Merkle-list */
  receiptChainHash?: Maybe<Scalars['ChainHash']['output']>;
  /**
   * True if you are actively staking with this account on the current daemon -
   * this may not yet have been updated if the staking key was changed recently
   */
  stakingActive: Scalars['Boolean']['output'];
  /** The timing associated with this account */
  timing: AccountTiming;
  /**
   * The token associated with this account
   * @deprecated Use tokenId
   */
  token: Scalars['TokenId']['output'];
  /** The token associated with this account */
  tokenId: Scalars['TokenId']['output'];
  /** The symbol for the token owned by this account, if there is one */
  tokenSymbol?: Maybe<Scalars['String']['output']>;
  /** Verification key associated with this account */
  verificationKey?: Maybe<AccountVerificationKeyWithHash>;
  /** The previous epoch lock hash of the chain which you are voting for */
  votingFor?: Maybe<Scalars['ChainHash']['output']>;
  /** The 8 field elements comprising the zkApp state associated with this account encoded as bignum strings */
  zkappState?: Maybe<Array<Scalars['FieldElem']['output']>>;
  /** The URI associated with this account, usually pointing to the zkApp source code */
  zkappUri?: Maybe<Scalars['String']['output']>;
};

/** Kind of authorization required */
export enum AccountAuthRequired {
  Either = 'Either',
  Impossible = 'Impossible',
  None = 'None',
  Proof = 'Proof',
  Signature = 'Signature'
}

export type AccountPermissions = {
  __typename?: 'AccountPermissions';
  /** Authorization required to access the account */
  access: AccountAuthRequired;
  /** Authorization required to edit the action state */
  editActionState: AccountAuthRequired;
  /** Authorization required to edit zkApp state */
  editState: AccountAuthRequired;
  /** Authorization required to increment the nonce */
  incrementNonce: AccountAuthRequired;
  /** Authorization required to receive tokens */
  receive: AccountAuthRequired;
  /** Authorization required to send tokens */
  send: AccountAuthRequired;
  /** Authorization required to set the delegate */
  setDelegate: AccountAuthRequired;
  /** Authorization required to change permissions */
  setPermissions: AccountAuthRequired;
  /** Authorization required to set the timing of the account */
  setTiming: AccountAuthRequired;
  /** Authorization required to set the token symbol */
  setTokenSymbol: AccountAuthRequired;
  /** Authorization required to set the verification key of the zkApp associated with the account */
  setVerificationKey: AccountAuthRequired;
  /** Authorization required to set the state hash the account is voting for */
  setVotingFor: AccountAuthRequired;
  /** Authorization required to change the URI of the zkApp associated with the account */
  setZkappUri: AccountAuthRequired;
};

export type AccountPrecondition = {
  __typename?: 'AccountPrecondition';
  actionState?: Maybe<Scalars['Field']['output']>;
  balance?: Maybe<BalanceInterval>;
  delegate?: Maybe<Scalars['PublicKey']['output']>;
  isNew?: Maybe<Scalars['Boolean']['output']>;
  nonce?: Maybe<NonceInterval>;
  provedState?: Maybe<Scalars['Boolean']['output']>;
  receiptChainHash?: Maybe<Scalars['Field']['output']>;
  state: Array<Maybe<Scalars['Field']['output']>>;
};

export type AccountPreconditionInput = {
  actionState?: InputMaybe<Scalars['Field']['input']>;
  balance?: InputMaybe<BalanceIntervalInput>;
  delegate?: InputMaybe<Scalars['PublicKey']['input']>;
  isNew?: InputMaybe<Scalars['Boolean']['input']>;
  nonce?: InputMaybe<NonceIntervalInput>;
  provedState?: InputMaybe<Scalars['Boolean']['input']>;
  receiptChainHash?: InputMaybe<Scalars['Field']['input']>;
  state: Array<InputMaybe<Scalars['Field']['input']>>;
};

export type AccountTiming = {
  __typename?: 'AccountTiming';
  /** The cliff amount for a time-locked account */
  cliffAmount?: Maybe<Scalars['Amount']['output']>;
  /** The cliff time for a time-locked account */
  cliffTime?: Maybe<Scalars['Globalslot']['output']>;
  /** The initial minimum balance for a time-locked account */
  initialMinimumBalance?: Maybe<Scalars['Balance']['output']>;
  /** The vesting increment for a time-locked account */
  vestingIncrement?: Maybe<Scalars['Amount']['output']>;
  /** The vesting period for a time-locked account */
  vestingPeriod?: Maybe<Scalars['Globalslot']['output']>;
};

export type AccountUpdateBody = {
  __typename?: 'AccountUpdateBody';
  actions: Array<Array<Scalars['Field']['output']>>;
  authorizationKind: AuthorizationKindStructured;
  balanceChange: BalanceChange;
  callData: Scalars['Field']['output'];
  callDepth: Scalars['Int']['output'];
  events: Array<Array<Scalars['Field']['output']>>;
  implicitAccountCreationFee: Scalars['Boolean']['output'];
  incrementNonce: Scalars['Boolean']['output'];
  mayUseToken: MayUseToken;
  preconditions: Preconditions;
  publicKey: Scalars['PublicKey']['output'];
  tokenId: Scalars['TokenId']['output'];
  update: AccountUpdateModification;
  useFullCommitment: Scalars['Boolean']['output'];
};

export type AccountUpdateBodyInput = {
  actions: Array<Array<Scalars['Field']['input']>>;
  authorizationKind: AuthorizationKindStructuredInput;
  balanceChange: BalanceChangeInput;
  callData: Scalars['Field']['input'];
  callDepth: Scalars['Int']['input'];
  events: Array<Array<Scalars['Field']['input']>>;
  implicitAccountCreationFee: Scalars['Boolean']['input'];
  incrementNonce: Scalars['Boolean']['input'];
  mayUseToken: MayUseTokenInput;
  preconditions: PreconditionsInput;
  publicKey: Scalars['PublicKey']['input'];
  tokenId: Scalars['TokenId']['input'];
  update: AccountUpdateModificationInput;
  useFullCommitment: Scalars['Boolean']['input'];
};

export type AccountUpdateModification = {
  __typename?: 'AccountUpdateModification';
  appState: Array<Maybe<Scalars['Field']['output']>>;
  delegate?: Maybe<Scalars['PublicKey']['output']>;
  permissions?: Maybe<Permissions>;
  timing?: Maybe<Timing>;
  tokenSymbol?: Maybe<Scalars['String']['output']>;
  verificationKey?: Maybe<VerificationKeyWithHash>;
  votingFor?: Maybe<Scalars['StateHash']['output']>;
  zkappUri?: Maybe<Scalars['String']['output']>;
};

export type AccountUpdateModificationInput = {
  appState: Array<InputMaybe<Scalars['Field']['input']>>;
  delegate?: InputMaybe<Scalars['PublicKey']['input']>;
  permissions?: InputMaybe<PermissionsInput>;
  timing?: InputMaybe<TimingInput>;
  tokenSymbol?: InputMaybe<Scalars['String']['input']>;
  verificationKey?: InputMaybe<VerificationKeyWithHashInput>;
  votingFor?: InputMaybe<Scalars['StateHash']['input']>;
  zkappUri?: InputMaybe<Scalars['String']['input']>;
};

/** Verification key with hash */
export type AccountVerificationKeyWithHash = {
  __typename?: 'AccountVerificationKeyWithHash';
  /** Hash of verification key */
  hash: Scalars['VerificationKeyHash']['output'];
  /** verification key in Base64 format */
  verificationKey: Scalars['VerificationKey']['output'];
};

export type ActionData = {
  __typename?: 'ActionData';
  accountUpdateId: Scalars['String']['output'];
  data: Array<Maybe<Scalars['String']['output']>>;
  transactionInfo?: Maybe<TransactionInfo>;
};

export type ActionFilterOptionsInput = {
  address: Scalars['String']['input'];
  endActionState?: InputMaybe<Scalars['String']['input']>;
  from?: InputMaybe<Scalars['Int']['input']>;
  fromActionState?: InputMaybe<Scalars['String']['input']>;
  status?: InputMaybe<BlockStatusFilter>;
  to?: InputMaybe<Scalars['Int']['input']>;
  tokenId?: InputMaybe<Scalars['String']['input']>;
};

export type ActionOutput = {
  __typename?: 'ActionOutput';
  actionData?: Maybe<Array<Maybe<ActionData>>>;
  actionState: ActionStates;
  blockInfo?: Maybe<BlockInfo>;
  transactionInfo?: Maybe<TransactionInfo>;
};

export type ActionStates = {
  __typename?: 'ActionStates';
  actionStateFive?: Maybe<Scalars['String']['output']>;
  actionStateFour?: Maybe<Scalars['String']['output']>;
  actionStateOne?: Maybe<Scalars['String']['output']>;
  actionStateThree?: Maybe<Scalars['String']['output']>;
  actionStateTwo?: Maybe<Scalars['String']['output']>;
};

export type AddAccountInput = {
  /** Password used to encrypt the new account */
  password: Scalars['String']['input'];
};

export type AddAccountPayload = {
  __typename?: 'AddAccountPayload';
  /** Details of created account */
  account: Account;
  /**
   * Public key of the created account
   * @deprecated use account field instead
   */
  publicKey: Scalars['PublicKey']['output'];
};

export type AddrsAndPorts = {
  __typename?: 'AddrsAndPorts';
  bindIp: Scalars['String']['output'];
  clientPort: Scalars['Int']['output'];
  externalIp: Scalars['String']['output'];
  libp2pPort: Scalars['Int']['output'];
  peer?: Maybe<Peer>;
};

/**
 * A total balance annotated with the amount that is currently unknown with the
 * invariant unknown <= total, as well as the currently liquid and locked balances.
 */
export type AnnotatedBalance = {
  __typename?: 'AnnotatedBalance';
  /** Block height at which balance was measured */
  blockHeight: Scalars['Length']['output'];
  /** The amount of MINA owned by the account which is currently available. Can be null if bootstrapping. */
  liquid?: Maybe<Scalars['Balance']['output']>;
  /** The amount of MINA owned by the account which is currently locked. Can be null if bootstrapping. */
  locked?: Maybe<Scalars['Balance']['output']>;
  /**
   * Hash of block at which balance was measured. Can be null if bootstrapping.
   * Guaranteed to be non-null for direct account lookup queries when not
   * bootstrapping. Can also be null when accessed as nested properties (eg. via delegators).
   */
  stateHash?: Maybe<Scalars['StateHash']['output']>;
  /** The amount of MINA owned by the account */
  total: Scalars['Balance']['output'];
  /** The amount of MINA owned by the account whose origin is currently unknown */
  unknown: Scalars['Balance']['output'];
};

export type Applied = {
  __typename?: 'Applied';
  applied: Scalars['Boolean']['output'];
};

export type AuthorizationKindStructured = {
  __typename?: 'AuthorizationKindStructured';
  isProved: Scalars['Boolean']['output'];
  isSigned: Scalars['Boolean']['output'];
  verificationKeyHash: Scalars['Field']['output'];
};

export type AuthorizationKindStructuredInput = {
  isProved: Scalars['Boolean']['input'];
  isSigned: Scalars['Boolean']['input'];
  verificationKeyHash: Scalars['Field']['input'];
};

export type BalanceChange = {
  __typename?: 'BalanceChange';
  magnitude: Scalars['CurrencyAmount']['output'];
  sgn: Scalars['Sign']['output'];
};

export type BalanceChangeInput = {
  magnitude: Scalars['CurrencyAmount']['input'];
  sgn: Scalars['Sign']['input'];
};

export type BalanceInterval = {
  __typename?: 'BalanceInterval';
  lower: Scalars['Balance']['output'];
  upper: Scalars['Balance']['output'];
};

export type BalanceIntervalInput = {
  lower: Scalars['Balance']['input'];
  upper: Scalars['Balance']['input'];
};

export type Block = {
  __typename?: 'Block';
  /** Count of user command transactions in the block */
  commandTransactionCount: Scalars['Int']['output'];
  /**
   * Public key of account that produced this block
   * @deprecated use creatorAccount field instead
   */
  creator: Scalars['PublicKey']['output'];
  /** Account that produced this block */
  creatorAccount: Account;
  protocolState: ProtocolState;
  /** Snark proof of blockchain state */
  protocolStateProof: ProtocolStateProof;
  snarkJobs: Array<CompletedWork>;
  /** Base58Check-encoded hash of the state after this block */
  stateHash: Scalars['StateHash']['output'];
  /** Experimental: Bigint field-element representation of stateHash */
  stateHashField: Scalars['StateHashAsDecimal']['output'];
  transactions: Transactions;
  /** Account that won the slot (Delegator/Staker) */
  winnerAccount: Account;
};

export type BlockInfo = {
  __typename?: 'BlockInfo';
  chainStatus: Scalars['String']['output'];
  distanceFromMaxBlockHeight: Scalars['Int']['output'];
  globalSlotSinceGenesis: Scalars['Int']['output'];
  globalSlotSinceHardfork: Scalars['Int']['output'];
  height: Scalars['Int']['output'];
  ledgerHash: Scalars['String']['output'];
  parentHash: Scalars['String']['output'];
  stateHash: Scalars['String']['output'];
  timestamp: Scalars['String']['output'];
};

export type BlockProducerTimings = {
  __typename?: 'BlockProducerTimings';
  /** Consensus time of the block that was used to determine the next block production time */
  generatedFromConsensusAt: ConsensusTimeGlobalSlot;
  /** Next block production global-slot-since-genesis */
  globalSlotSinceGenesis: Array<Scalars['Globalslot']['output']>;
  /** Next block production time */
  times: Array<ConsensusTime>;
};

/** Archive node types */
export enum BlockStatusFilter {
  All = 'ALL',
  Canonical = 'CANONICAL',
  Pending = 'PENDING'
}

export type BlockchainState = {
  __typename?: 'BlockchainState';
  /** A reference to how the block header refers to the body of the block as a hex-encoded string */
  bodyReference: Scalars['BodyReference']['output'];
  /** date (stringified Unix time - number of milliseconds since January 1, 1970) */
  date: Scalars['BlockTime']['output'];
  /** Base58Check-encoded hash of the snarked ledger */
  snarkedLedgerHash: Scalars['LedgerHash']['output'];
  /** Base58Check-encoded hash of the staged ledger hash's aux_hash */
  stagedLedgerAuxHash: Scalars['StagedLedgerAuxHash']['output'];
  /** Base58Check-encoded hash of the staged ledger hash's main ledger hash */
  stagedLedgerHash: Scalars['LedgerHash']['output'];
  /** Base58Check-encoded staged ledger hash's pending_coinbase_aux */
  stagedLedgerPendingCoinbaseAux: Scalars['PendingCoinbaseAuxHash']['output'];
  /** Base58Check-encoded hash of the staged ledger hash's pending_coinbase_hash */
  stagedLedgerPendingCoinbaseHash: Scalars['PendingCoinbaseHash']['output'];
  /**
   * Block finished a staged ledger, and a proof was emitted from it and included
   * into this block's proof. If there is no transition frontier available or no
   * block found, this will return null.
   */
  stagedLedgerProofEmitted?: Maybe<Scalars['Boolean']['output']>;
  /**
   * utcDate (stringified Unix time - number of milliseconds since January 1,
   * 1970). Time offsets are adjusted to reflect true wall-clock time instead of genesis time.
   */
  utcDate: Scalars['BlockTime']['output'];
};

/** Status for whenever the blockchain is reorganized */
export enum ChainReorganizationStatus {
  Changed = 'CHANGED'
}

/** Completed snark works */
export type CompletedWork = {
  __typename?: 'CompletedWork';
  /** Amount the prover is paid for the snark work */
  fee: Scalars['Fee']['output'];
  /** Public key of the prover */
  prover: Scalars['PublicKey']['output'];
  /** Unique identifier for the snark work purchased */
  workIds: Array<Scalars['Int']['output']>;
};

export type ConsensusConfiguration = {
  __typename?: 'ConsensusConfiguration';
  acceptableNetworkDelay: Scalars['Int']['output'];
  delta: Scalars['Int']['output'];
  epochDuration: Scalars['Int']['output'];
  genesisStateTimestamp: Scalars['Time']['output'];
  k: Scalars['Int']['output'];
  slotDuration: Scalars['Int']['output'];
  slotsPerEpoch: Scalars['Int']['output'];
};

export type ConsensusState = {
  __typename?: 'ConsensusState';
  /** The block producer public key that created this block */
  blockCreator: Scalars['PublicKey']['output'];
  /** Height of the blockchain at this block */
  blockHeight: Scalars['Length']['output'];
  /** The public key that is responsible for winning this block (including delegations) */
  blockStakeWinner: Scalars['PublicKey']['output'];
  /**
   * Length of the blockchain at this block
   * @deprecated use blockHeight instead
   */
  blockchainLength: Scalars['Length']['output'];
  coinbaseReceiever: Scalars['PublicKey']['output'];
  /** Epoch in which this block was created */
  epoch: Scalars['Epoch']['output'];
  epochCount: Scalars['Length']['output'];
  hasAncestorInSameCheckpointWindow: Scalars['Boolean']['output'];
  lastVrfOutput: Scalars['String']['output'];
  minWindowDensity: Scalars['Length']['output'];
  nextEpochData: NextEpochData;
  /** Slot in which this block was created */
  slot: Scalars['Slot']['output'];
  /** Slot since genesis (across all hard-forks) */
  slotSinceGenesis: Scalars['Globalslot']['output'];
  stakingEpochData: StakingEpochData;
  /** Whether or not this coinbase was "supercharged", ie. created by an account that has no locked tokens */
  superchargedCoinbase: Scalars['Boolean']['output'];
  /** Total currency in circulation at this block */
  totalCurrency: Scalars['Amount']['output'];
};

export type ConsensusTime = {
  __typename?: 'ConsensusTime';
  endTime: Scalars['BlockTime']['output'];
  epoch: Scalars['UInt32']['output'];
  globalSlot: Scalars['Globalslot']['output'];
  slot: Scalars['UInt32']['output'];
  startTime: Scalars['BlockTime']['output'];
};

/** Consensus time and the corresponding global slot since genesis */
export type ConsensusTimeGlobalSlot = {
  __typename?: 'ConsensusTimeGlobalSlot';
  /** Time in terms of slot number in an epoch, start and end time of the slot since UTC epoch */
  consensusTime: ConsensusTime;
  globalSlotSinceGenesis: Scalars['Globalslot']['output'];
};

export type Control = {
  __typename?: 'Control';
  proof?: Maybe<Scalars['ZkappProof']['output']>;
  signature?: Maybe<Scalars['Signature']['output']>;
};

export type ControlInput = {
  proof?: InputMaybe<Scalars['ZkappProof']['input']>;
  signature?: InputMaybe<Scalars['Signature']['input']>;
};

export type CreateHdAccountInput = {
  /** Index of the account in hardware wallet */
  index: Scalars['UInt32']['input'];
};

export type CurrencyAmountInterval = {
  __typename?: 'CurrencyAmountInterval';
  lower: Scalars['CurrencyAmount']['output'];
  upper: Scalars['CurrencyAmount']['output'];
};

export type CurrencyAmountIntervalInput = {
  lower: Scalars['CurrencyAmount']['input'];
  upper: Scalars['CurrencyAmount']['input'];
};

export type DaemonStatus = {
  __typename?: 'DaemonStatus';
  addrsAndPorts: AddrsAndPorts;
  blockProductionKeys: Array<Scalars['String']['output']>;
  blockchainLength?: Maybe<Scalars['Int']['output']>;
  catchupStatus?: Maybe<Array<Scalars['String']['output']>>;
  chainId: Scalars['String']['output'];
  coinbaseReceiver?: Maybe<Scalars['String']['output']>;
  commitId: Scalars['String']['output'];
  confDir: Scalars['String']['output'];
  consensusConfiguration: ConsensusConfiguration;
  consensusMechanism: Scalars['String']['output'];
  consensusTimeBestTip?: Maybe<ConsensusTime>;
  consensusTimeNow: ConsensusTime;
  globalSlotSinceGenesisBestTip?: Maybe<Scalars['Int']['output']>;
  highestBlockLengthReceived: Scalars['Int']['output'];
  highestUnvalidatedBlockLengthReceived: Scalars['Int']['output'];
  histograms?: Maybe<Histograms>;
  ledgerMerkleRoot?: Maybe<Scalars['String']['output']>;
  metrics: Metrics;
  nextBlockProduction?: Maybe<BlockProducerTimings>;
  numAccounts?: Maybe<Scalars['Int']['output']>;
  peers: Array<Peer>;
  snarkWorkFee: Scalars['Int']['output'];
  snarkWorker?: Maybe<Scalars['String']['output']>;
  stateHash?: Maybe<Scalars['String']['output']>;
  syncStatus: SyncStatus;
  uptimeSecs: Scalars['Int']['output'];
  userCommandsSent: Scalars['Int']['output'];
};

export type DeleteAccountInput = {
  /** Public key of account to be deleted */
  publicKey: Scalars['PublicKey']['input'];
};

export type DeleteAccountPayload = {
  __typename?: 'DeleteAccountPayload';
  /** Public key of the deleted account */
  publicKey: Scalars['PublicKey']['output'];
};

export type EpochDataPrecondition = {
  __typename?: 'EpochDataPrecondition';
  epochLength?: Maybe<LengthInterval>;
  ledger: EpochLedgerPrecondition;
  lockCheckpoint?: Maybe<Scalars['Field']['output']>;
  seed?: Maybe<Scalars['Field']['output']>;
  startCheckpoint?: Maybe<Scalars['Field']['output']>;
};

export type EpochDataPreconditionInput = {
  epochLength?: InputMaybe<LengthIntervalInput>;
  ledger: EpochLedgerPreconditionInput;
  lockCheckpoint?: InputMaybe<Scalars['Field']['input']>;
  seed?: InputMaybe<Scalars['Field']['input']>;
  startCheckpoint?: InputMaybe<Scalars['Field']['input']>;
};

export type EpochLedgerPrecondition = {
  __typename?: 'EpochLedgerPrecondition';
  hash?: Maybe<Scalars['Field']['output']>;
  totalCurrency?: Maybe<CurrencyAmountInterval>;
};

export type EpochLedgerPreconditionInput = {
  hash?: InputMaybe<Scalars['Field']['input']>;
  totalCurrency?: InputMaybe<CurrencyAmountIntervalInput>;
};

export type EventData = {
  __typename?: 'EventData';
  data: Array<Maybe<Scalars['String']['output']>>;
  transactionInfo?: Maybe<TransactionInfo>;
};

export type EventFilterOptionsInput = {
  address: Scalars['String']['input'];
  from?: InputMaybe<Scalars['Int']['input']>;
  status?: InputMaybe<BlockStatusFilter>;
  to?: InputMaybe<Scalars['Int']['input']>;
  tokenId?: InputMaybe<Scalars['String']['input']>;
};

export type EventOutput = {
  __typename?: 'EventOutput';
  blockInfo?: Maybe<BlockInfo>;
  eventData?: Maybe<Array<Maybe<EventData>>>;
};

export type ExportLogsPayload = {
  __typename?: 'ExportLogsPayload';
  /** Tar archive containing logs */
  exportLogs: TarFile;
};

export type FeePayerBody = {
  __typename?: 'FeePayerBody';
  fee: Scalars['Fee']['output'];
  nonce: Scalars['UInt32']['output'];
  publicKey: Scalars['PublicKey']['output'];
  validUntil?: Maybe<Scalars['UInt32']['output']>;
};

export type FeePayerBodyInput = {
  fee: Scalars['Fee']['input'];
  nonce: Scalars['UInt32']['input'];
  publicKey: Scalars['PublicKey']['input'];
  validUntil?: InputMaybe<Scalars['UInt32']['input']>;
};

export type FeeTransfer = {
  __typename?: 'FeeTransfer';
  /** Amount that the recipient is paid in this fee transfer */
  fee: Scalars['Fee']['output'];
  /** Public key of fee transfer recipient */
  recipient: Scalars['PublicKey']['output'];
  /**
   * Fee_transfer|Fee_transfer_via_coinbase Snark worker fees deducted from the
   * coinbase amount are of type 'Fee_transfer_via_coinbase', rest are deducted
   * from transaction fees
   */
  type: Scalars['FeeTransferType']['output'];
};

export type GenesisConstants = {
  __typename?: 'GenesisConstants';
  /** The fee charged to create a new account */
  accountCreationFee: Scalars['Fee']['output'];
  /** The amount received as a coinbase reward for producing a block */
  coinbase: Scalars['Amount']['output'];
};

export type GlobalSlotInterval = {
  __typename?: 'GlobalSlotInterval';
  lower: Scalars['UInt32']['output'];
  upper: Scalars['UInt32']['output'];
};

export type GlobalSlotIntervalInput = {
  lower: Scalars['UInt32']['input'];
  upper: Scalars['UInt32']['input'];
};

export type Histogram = {
  __typename?: 'Histogram';
  intervals: Array<Interval>;
  overflow: Scalars['Int']['output'];
  underflow: Scalars['Int']['output'];
  values: Array<Scalars['Int']['output']>;
};

export type Histograms = {
  __typename?: 'Histograms';
  acceptedTransitionLocalLatency?: Maybe<Histogram>;
  acceptedTransitionRemoteLatency?: Maybe<Histogram>;
  externalTransitionLatency?: Maybe<Histogram>;
  rpcTimings: RpcTimings;
  snarkWorkerMergeTime?: Maybe<Histogram>;
  snarkWorkerTransitionTime?: Maybe<Histogram>;
};

export type ImportAccountPayload = {
  __typename?: 'ImportAccountPayload';
  /** True if the account had already been imported */
  alreadyImported: Scalars['Boolean']['output'];
  /** The public key of the imported account */
  publicKey: Scalars['PublicKey']['output'];
  success: Scalars['Boolean']['output'];
};

export type Interval = {
  __typename?: 'Interval';
  start: Scalars['Span']['output'];
  stop: Scalars['Span']['output'];
};

export type LengthInterval = {
  __typename?: 'LengthInterval';
  lower: Scalars['UInt32']['output'];
  upper: Scalars['UInt32']['output'];
};

export type LengthIntervalInput = {
  lower: Scalars['UInt32']['input'];
  upper: Scalars['UInt32']['input'];
};

export type LockInput = {
  /** Public key specifying which account to lock */
  publicKey: Scalars['PublicKey']['input'];
};

export type LockPayload = {
  __typename?: 'LockPayload';
  /** Details of locked account */
  account: Account;
  /** Public key of the locked account */
  publicKey: Scalars['PublicKey']['output'];
};

export type MayUseToken = {
  __typename?: 'MayUseToken';
  inheritFromParent: Scalars['Boolean']['output'];
  parentsOwnToken: Scalars['Boolean']['output'];
};

export type MayUseTokenInput = {
  inheritFromParent: Scalars['Boolean']['input'];
  parentsOwnToken: Scalars['Boolean']['input'];
};

export type MerklePathElement = {
  __typename?: 'MerklePathElement';
  left?: Maybe<Scalars['FieldElem']['output']>;
  right?: Maybe<Scalars['FieldElem']['output']>;
};

export type Metrics = {
  __typename?: 'Metrics';
  blockProductionDelay: Array<Scalars['Int']['output']>;
  transactionPoolDiffBroadcasted: Scalars['Int']['output'];
  transactionPoolDiffReceived: Scalars['Int']['output'];
  transactionPoolSize: Scalars['Int']['output'];
  transactionsAddedToPool: Scalars['Int']['output'];
};

/** Network identifiers for another protocol participant */
export type NetworkPeer = {
  /** IP address of the remote host */
  host: Scalars['String']['input'];
  libp2pPort: Scalars['Int']['input'];
  /** base58-encoded peer ID */
  peerId: Scalars['String']['input'];
};

export type NetworkPeerPayload = {
  __typename?: 'NetworkPeerPayload';
  /** IP address of the remote host */
  host: Scalars['InetAddr']['output'];
  libp2pPort: Scalars['Int']['output'];
  /** base58-encoded peer ID */
  peerId: Scalars['String']['output'];
};

export type NetworkPrecondition = {
  __typename?: 'NetworkPrecondition';
  blockchainLength?: Maybe<LengthInterval>;
  globalSlotSinceGenesis?: Maybe<GlobalSlotInterval>;
  minWindowDensity?: Maybe<LengthInterval>;
  nextEpochData: EpochDataPrecondition;
  snarkedLedgerHash?: Maybe<Scalars['Field']['output']>;
  stakingEpochData: EpochDataPrecondition;
  totalCurrency?: Maybe<CurrencyAmountInterval>;
};

export type NetworkPreconditionInput = {
  blockchainLength?: InputMaybe<LengthIntervalInput>;
  globalSlotSinceGenesis?: InputMaybe<GlobalSlotIntervalInput>;
  minWindowDensity?: InputMaybe<LengthIntervalInput>;
  nextEpochData: EpochDataPreconditionInput;
  snarkedLedgerHash?: InputMaybe<Scalars['Field']['input']>;
  stakingEpochData: EpochDataPreconditionInput;
  totalCurrency?: InputMaybe<CurrencyAmountIntervalInput>;
};

export type NextEpochData = {
  __typename?: 'NextEpochData';
  epochLength: Scalars['Length']['output'];
  ledger: EpochLedger;
  lockCheckpoint: Scalars['String']['output'];
  seed: Scalars['EpochSeed']['output'];
  startCheckpoint: Scalars['StateHash']['output'];
};

export type NonceInterval = {
  __typename?: 'NonceInterval';
  lower: Scalars['UInt32']['output'];
  upper: Scalars['UInt32']['output'];
};

export type NonceIntervalInput = {
  lower: Scalars['UInt32']['input'];
  upper: Scalars['UInt32']['input'];
};

export type Peer = {
  __typename?: 'Peer';
  host: Scalars['String']['output'];
  libp2pPort: Scalars['Int']['output'];
  peerId: Scalars['String']['output'];
};

/** Snark work bundles that are not available in the pool yet */
export type PendingSnarkWork = {
  __typename?: 'PendingSnarkWork';
  /** Work bundle with one or two snark work */
  workBundle: Array<WorkDescription>;
};

export type Permissions = {
  __typename?: 'Permissions';
  access: Scalars['AuthRequired']['output'];
  editActionState: Scalars['AuthRequired']['output'];
  editState: Scalars['AuthRequired']['output'];
  incrementNonce: Scalars['AuthRequired']['output'];
  receive: Scalars['AuthRequired']['output'];
  send: Scalars['AuthRequired']['output'];
  setDelegate: Scalars['AuthRequired']['output'];
  setPermissions: Scalars['AuthRequired']['output'];
  setTiming: Scalars['AuthRequired']['output'];
  setTokenSymbol: Scalars['AuthRequired']['output'];
  setVerificationKey: Scalars['AuthRequired']['output'];
  setVotingFor: Scalars['AuthRequired']['output'];
  setZkappUri: Scalars['AuthRequired']['output'];
};

export type PermissionsInput = {
  access: Scalars['AuthRequired']['input'];
  editActionState: Scalars['AuthRequired']['input'];
  editState: Scalars['AuthRequired']['input'];
  incrementNonce: Scalars['AuthRequired']['input'];
  receive: Scalars['AuthRequired']['input'];
  send: Scalars['AuthRequired']['input'];
  setDelegate: Scalars['AuthRequired']['input'];
  setPermissions: Scalars['AuthRequired']['input'];
  setTiming: Scalars['AuthRequired']['input'];
  setTokenSymbol: Scalars['AuthRequired']['input'];
  setVerificationKey: Scalars['AuthRequired']['input'];
  setVotingFor: Scalars['AuthRequired']['input'];
  setZkappUri: Scalars['AuthRequired']['input'];
};

export type Preconditions = {
  __typename?: 'Preconditions';
  account: AccountPrecondition;
  network: NetworkPrecondition;
  validWhile?: Maybe<GlobalSlotInterval>;
};

export type PreconditionsInput = {
  account: AccountPreconditionInput;
  network: NetworkPreconditionInput;
  validWhile?: InputMaybe<GlobalSlotIntervalInput>;
};

export type ProtocolState = {
  __typename?: 'ProtocolState';
  /** State which is agnostic of a particular consensus algorithm */
  blockchainState: BlockchainState;
  /** State specific to the minaboros Proof of Stake consensus algorithm */
  consensusState: ConsensusState;
  /** Base58Check-encoded hash of the previous state */
  previousStateHash: Scalars['StateHash']['output'];
};

export type ReloadAccountsPayload = {
  __typename?: 'ReloadAccountsPayload';
  /** True when the reload was successful */
  success: Scalars['Boolean']['output'];
};

export type RpcPair = {
  __typename?: 'RpcPair';
  dispatch?: Maybe<Histogram>;
  impl?: Maybe<Histogram>;
};

export type RpcTimings = {
  __typename?: 'RpcTimings';
  answerSyncLedgerQuery: RpcPair;
  getAncestry: RpcPair;
  getStagedLedgerAux: RpcPair;
  getTransitionChain: RpcPair;
  getTransitionChainProof: RpcPair;
};

export type SendDelegationInput = {
  /** Fee amount in order to send a stake delegation */
  fee: Scalars['UInt64']['input'];
  /** Public key of sender of a stake delegation */
  from: Scalars['PublicKey']['input'];
  /** Short arbitrary message provided by the sender */
  memo?: InputMaybe<Scalars['String']['input']>;
  /** Should only be set when cancelling transactions, otherwise a nonce is determined automatically */
  nonce?: InputMaybe<Scalars['UInt32']['input']>;
  /** Public key of the account being delegated to */
  to: Scalars['PublicKey']['input'];
  /** The global slot since genesis after which this transaction cannot be applied */
  validUntil?: InputMaybe<Scalars['UInt32']['input']>;
};

export type SendDelegationPayload = {
  __typename?: 'SendDelegationPayload';
  /** Delegation change that was sent */
  delegation: UserCommand;
};

export type SendPaymentInput = {
  /** Amount of MINA to send to receiver */
  amount: Scalars['UInt64']['input'];
  /** Fee amount in order to send payment */
  fee: Scalars['UInt64']['input'];
  /** Public key of sender of payment */
  from: Scalars['PublicKey']['input'];
  /** Short arbitrary message provided by the sender */
  memo?: InputMaybe<Scalars['String']['input']>;
  /** Should only be set when cancelling transactions, otherwise a nonce is determined automatically */
  nonce?: InputMaybe<Scalars['UInt32']['input']>;
  /** Public key of recipient of payment */
  to: Scalars['PublicKey']['input'];
  /** The global slot since genesis after which this transaction cannot be applied */
  validUntil?: InputMaybe<Scalars['UInt32']['input']>;
};

export type SendPaymentPayload = {
  __typename?: 'SendPaymentPayload';
  /** Payment that was sent */
  payment: UserCommand;
};

export type SendRosettaTransactionPayload = {
  __typename?: 'SendRosettaTransactionPayload';
  /** Command that was sent */
  userCommand: UserCommand;
};

export type SendZkappInput = {
  /** zkApp command structure representing the transaction */
  zkappCommand: ZkappCommandInput;
};

export type SendZkappPayload = {
  __typename?: 'SendZkappPayload';
  /** zkApp transaction that was sent */
  zkapp: ZkappCommandResult;
};

export type SetCoinbaseReceiverInput = {
  /**
   * Public key of the account to receive coinbases. Block production keys will
   * receive the coinbases if omitted. Warning: If the key is from a zkApp account,
   * the account's receive permission must be None.
   */
  publicKey?: InputMaybe<Scalars['PublicKey']['input']>;
};

export type SetCoinbaseReceiverPayload = {
  __typename?: 'SetCoinbaseReceiverPayload';
  /** Returns the public key that will receive coinbase, or none if it will be the block producer */
  currentCoinbaseReceiver?: Maybe<Scalars['PublicKey']['output']>;
  /** Returns the public key that was receiving coinbases previously, or none if it was the block producer */
  lastCoinbaseReceiver?: Maybe<Scalars['PublicKey']['output']>;
};

export type SetConnectionGatingConfigInput = {
  /** Peers we will never allow connections from (unless they are also trusted!) */
  bannedPeers: Array<NetworkPeer>;
  /** If true, no connections will be allowed unless they are from a trusted peer */
  isolate: Scalars['Boolean']['input'];
  /** Peers we will always allow connections from */
  trustedPeers: Array<NetworkPeer>;
};

export type SetConnectionGatingConfigPayload = {
  __typename?: 'SetConnectionGatingConfigPayload';
  /** Peers we will never allow connections from (unless they are also trusted!) */
  bannedPeers: Array<NetworkPeerPayload>;
  /** If true, no connections will be allowed unless they are from a trusted peer */
  isolate: Scalars['Boolean']['output'];
  /** Peers we will always allow connections from */
  trustedPeers: Array<NetworkPeerPayload>;
};

export type SetSnarkWorkFee = {
  /** Fee to get rewarded for producing snark work */
  fee: Scalars['UInt64']['input'];
};

export type SetSnarkWorkFeePayload = {
  __typename?: 'SetSnarkWorkFeePayload';
  /** Returns the last fee set to do snark work */
  lastFee: Scalars['Fee']['output'];
};

export type SetSnarkWorkerInput = {
  /**
   * Public key you wish to start snark-working on; null to stop doing any snark
   * work. Warning: If the key is from a zkApp account, the account's receive
   * permission must be None.
   */
  publicKey?: InputMaybe<Scalars['PublicKey']['input']>;
};

export type SetSnarkWorkerPayload = {
  __typename?: 'SetSnarkWorkerPayload';
  /** Returns the last public key that was designated for snark work */
  lastSnarkWorker?: Maybe<Scalars['PublicKey']['output']>;
};

/** A cryptographic signature -- you must provide either field+scalar or rawSignature */
export type SignatureInput = {
  /** Field component of signature */
  field?: InputMaybe<Scalars['String']['input']>;
  /** Raw encoded signature */
  rawSignature?: InputMaybe<Scalars['String']['input']>;
  /** Scalar component of signature */
  scalar?: InputMaybe<Scalars['String']['input']>;
};

/** Signed fee */
export type SignedFee = {
  __typename?: 'SignedFee';
  /** Fee */
  feeMagnitude: Scalars['Amount']['output'];
  /** +/- */
  sign: Sign;
};

export type SnarkWorker = {
  __typename?: 'SnarkWorker';
  /** Account of the current snark worker */
  account: Account;
  /** Fee that snark worker is charging to generate a snark proof */
  fee: Scalars['Fee']['output'];
  /**
   * Public key of current snark worker
   * @deprecated use account field instead
   */
  key: Scalars['PublicKey']['output'];
};

export type StakingEpochData = {
  __typename?: 'StakingEpochData';
  epochLength: Scalars['Length']['output'];
  ledger: EpochLedger;
  lockCheckpoint: Scalars['String']['output'];
  seed: Scalars['EpochSeed']['output'];
  startCheckpoint: Scalars['StateHash']['output'];
};

/** Sync status of daemon */
export enum SyncStatus {
  Bootstrap = 'BOOTSTRAP',
  Catchup = 'CATCHUP',
  Connecting = 'CONNECTING',
  Listening = 'LISTENING',
  Offline = 'OFFLINE',
  Synced = 'SYNCED'
}

export type TarFile = {
  __typename?: 'TarFile';
  tarfile: Scalars['String']['output'];
};

export type Timing = {
  __typename?: 'Timing';
  cliffAmount: Scalars['CurrencyAmount']['output'];
  cliffTime: Scalars['GlobalSlot']['output'];
  initialMinimumBalance: Scalars['Balance']['output'];
  vestingIncrement: Scalars['CurrencyAmount']['output'];
  vestingPeriod: Scalars['GlobalSlot']['output'];
};

export type TimingInput = {
  cliffAmount: Scalars['CurrencyAmount']['input'];
  cliffTime: Scalars['GlobalSlot']['input'];
  initialMinimumBalance: Scalars['Balance']['input'];
  vestingIncrement: Scalars['CurrencyAmount']['input'];
  vestingPeriod: Scalars['GlobalSlot']['input'];
};

export type TransactionInfo = {
  __typename?: 'TransactionInfo';
  authorizationKind: Scalars['String']['output'];
  hash: Scalars['String']['output'];
  memo: Scalars['String']['output'];
  status: Scalars['String']['output'];
};

/** Status of a transaction */
export enum TransactionStatus {
  /** A transaction that is on the longest chain */
  Included = 'INCLUDED',
  /** A transaction either in the transition frontier or in transaction pool but is not on the longest chain */
  Pending = 'PENDING',
  /** The transaction has either been snarked, reached finality through consensus or has been dropped */
  Unknown = 'UNKNOWN'
}

/** Different types of transactions in a block */
export type Transactions = {
  __typename?: 'Transactions';
  /** Amount of MINA granted to the producer of this block */
  coinbase: Scalars['Amount']['output'];
  /** Account to which the coinbase for this block was granted */
  coinbaseReceiverAccount?: Maybe<Account>;
  /** List of fee transfers included in this block */
  feeTransfer: Array<FeeTransfer>;
  /** List of user commands (payments and stake delegations) included in this block */
  userCommands: Array<UserCommand>;
  /** List of zkApp commands included in this block */
  zkappCommands: Array<ZkappCommandResult>;
};

export type TrustStatusPayload = {
  __typename?: 'TrustStatusPayload';
  /** Banned status */
  bannedStatus?: Maybe<Scalars['Time']['output']>;
  /** IP address */
  ipAddr: Scalars['InetAddr']['output'];
  /** libp2p Peer ID */
  peerId: Scalars['String']['output'];
  /** Trust score */
  trust: Scalars['Float']['output'];
};

export type UnlockInput = {
  /** Password for the account to be unlocked */
  password: Scalars['String']['input'];
  /** Public key specifying which account to unlock */
  publicKey: Scalars['PublicKey']['input'];
};

export type UnlockPayload = {
  __typename?: 'UnlockPayload';
  /** Details of unlocked account */
  account: Account;
  /**
   * Public key of the unlocked account
   * @deprecated use account field instead
   */
  publicKey: Scalars['PublicKey']['output'];
};

/** Common interface for user commands */
export type UserCommand = {
  /** Amount that the source is sending to receiver - 0 for commands that are not associated with an amount */
  amount: Scalars['Amount']['output'];
  /** null is no failure, reason for failure otherwise. */
  failureReason?: Maybe<Scalars['TransactionStatusFailure']['output']>;
  /** Fee that the fee-payer is willing to pay for making the transaction */
  fee: Scalars['Fee']['output'];
  /** Account that pays the fees for the command */
  feePayer: Account;
  /** Token used to pay the fee */
  feeToken: Scalars['TokenId']['output'];
  /**
   * Public key of the sender
   * @deprecated use feePayer field instead
   */
  from: Scalars['PublicKey']['output'];
  /**
   * Account of the sender
   * @deprecated use feePayer field instead
   */
  fromAccount: Account;
  hash: Scalars['TransactionHash']['output'];
  id: Scalars['TransactionId']['output'];
  /**
   * If true, this represents a delegation of stake, otherwise it is a payment
   * @deprecated use kind field instead
   */
  isDelegation: Scalars['Boolean']['output'];
  /** String describing the kind of user command */
  kind: Scalars['UserCommandKind']['output'];
  /** Short arbitrary message provided by the sender */
  memo: Scalars['String']['output'];
  /** Sequence number of command for the fee-payer's account */
  nonce: Scalars['Int']['output'];
  /** Account that the command applies to */
  receiver: Account;
  /** Account that the command is sent from */
  source: Account;
  /**
   * Public key of the receiver
   * @deprecated use receiver field instead
   */
  to: Scalars['PublicKey']['output'];
  /**
   * Account of the receiver
   * @deprecated use receiver field instead
   */
  toAccount: Account;
  /** Token used by the command */
  token: Scalars['TokenId']['output'];
  /** The global slot number after which this transaction cannot be applied */
  validUntil: Scalars['Globalslot']['output'];
};

export type UserCommandDelegation = UserCommand & {
  __typename?: 'UserCommandDelegation';
  /** Amount that the source is sending to receiver; 0 for commands without an associated amount */
  amount: Scalars['Amount']['output'];
  delegatee: Account;
  delegator: Account;
  /** null is no failure or status unknown, reason for failure otherwise. */
  failureReason?: Maybe<Scalars['TransactionStatusFailure']['output']>;
  /** Fee that the fee-payer is willing to pay for making the transaction */
  fee: Scalars['Fee']['output'];
  /** Account that pays the fees for the command */
  feePayer: Account;
  /** Token used to pay the fee */
  feeToken: Scalars['TokenId']['output'];
  /**
   * Public key of the sender
   * @deprecated use feePayer field instead
   */
  from: Scalars['PublicKey']['output'];
  /**
   * Account of the sender
   * @deprecated use feePayer field instead
   */
  fromAccount: Account;
  hash: Scalars['TransactionHash']['output'];
  id: Scalars['TransactionId']['output'];
  /**
   * If true, this command represents a delegation of stake
   * @deprecated use kind field instead
   */
  isDelegation: Scalars['Boolean']['output'];
  /** String describing the kind of user command */
  kind: Scalars['UserCommandKind']['output'];
  /** A short message from the sender, encoded with Base58Check, version byte=0x14; byte 2 of the decoding is the message length */
  memo: Scalars['String']['output'];
  /** Sequence number of command for the fee-payer's account */
  nonce: Scalars['Int']['output'];
  /** Account that the command applies to */
  receiver: Account;
  /** Account that the command is sent from */
  source: Account;
  /**
   * Public key of the receiver
   * @deprecated use receiver field instead
   */
  to: Scalars['PublicKey']['output'];
  /**
   * Account of the receiver
   * @deprecated use receiver field instead
   */
  toAccount: Account;
  /** Token used for the transaction */
  token: Scalars['TokenId']['output'];
  /** The global slot number after which this transaction cannot be applied */
  validUntil: Scalars['Globalslot']['output'];
};

export type UserCommandPayment = UserCommand & {
  __typename?: 'UserCommandPayment';
  /** Amount that the source is sending to receiver; 0 for commands without an associated amount */
  amount: Scalars['Amount']['output'];
  /** null is no failure or status unknown, reason for failure otherwise. */
  failureReason?: Maybe<Scalars['TransactionStatusFailure']['output']>;
  /** Fee that the fee-payer is willing to pay for making the transaction */
  fee: Scalars['Fee']['output'];
  /** Account that pays the fees for the command */
  feePayer: Account;
  /** Token used to pay the fee */
  feeToken: Scalars['TokenId']['output'];
  /**
   * Public key of the sender
   * @deprecated use feePayer field instead
   */
  from: Scalars['PublicKey']['output'];
  /**
   * Account of the sender
   * @deprecated use feePayer field instead
   */
  fromAccount: Account;
  hash: Scalars['TransactionHash']['output'];
  id: Scalars['TransactionId']['output'];
  /**
   * If true, this command represents a delegation of stake
   * @deprecated use kind field instead
   */
  isDelegation: Scalars['Boolean']['output'];
  /** String describing the kind of user command */
  kind: Scalars['UserCommandKind']['output'];
  /** A short message from the sender, encoded with Base58Check, version byte=0x14; byte 2 of the decoding is the message length */
  memo: Scalars['String']['output'];
  /** Sequence number of command for the fee-payer's account */
  nonce: Scalars['Int']['output'];
  /** Account that the command applies to */
  receiver: Account;
  /** Account that the command is sent from */
  source: Account;
  /**
   * Public key of the receiver
   * @deprecated use receiver field instead
   */
  to: Scalars['PublicKey']['output'];
  /**
   * Account of the receiver
   * @deprecated use receiver field instead
   */
  toAccount: Account;
  /** Token used for the transaction */
  token: Scalars['TokenId']['output'];
  /** The global slot number after which this transaction cannot be applied */
  validUntil: Scalars['Globalslot']['output'];
};

export type VerificationKeyWithHash = {
  __typename?: 'VerificationKeyWithHash';
  data: Scalars['VerificationKey']['output'];
  hash: Scalars['Field']['output'];
};

export type VerificationKeyWithHashInput = {
  data: Scalars['VerificationKey']['input'];
  hash: Scalars['Field']['input'];
};

/** A witness to a vrf evaluation, which may be externally verified */
export type VrfEvaluation = {
  __typename?: 'VrfEvaluation';
  c: Scalars['VrfScalar']['output'];
  message: VrfMessage;
  publicKey: Scalars['PublicKey']['output'];
  s: Scalars['VrfScalar']['output'];
  /** A group element represented as 2 field elements */
  scaledMessageHash: Array<Scalars['String']['output']>;
  /** Whether the threshold to produce a block was met, if specified */
  thresholdMet?: Maybe<Scalars['Boolean']['output']>;
  /** The vrf output derived from the evaluation witness. If null, the vrf witness was invalid. */
  vrfOutput?: Maybe<Scalars['VrfOutputTruncated']['output']>;
  /**
   * The vrf output derived from the evaluation witness, as a fraction. This
   * represents a won slot if vrfOutputFractional <= (1 - (1 /
   * 4)^(delegated_balance / total_stake)). If null, the vrf witness was invalid.
   */
  vrfOutputFractional?: Maybe<Scalars['Float']['output']>;
  vrfThreshold?: Maybe<VrfThreshold>;
};


/** A witness to a vrf evaluation, which may be externally verified */
export type VrfEvaluationThresholdMetArgs = {
  input?: InputMaybe<VrfThresholdInput>;
};

/** The witness to a vrf evaluation */
export type VrfEvaluationInput = {
  c: Scalars['String']['input'];
  message: VrfMessageInput;
  publicKey: Scalars['PublicKey']['input'];
  s: Scalars['String']['input'];
  scaledMessageHash: Array<Scalars['String']['input']>;
  vrfThreshold?: InputMaybe<VrfThresholdInput>;
};

/** The inputs to a vrf evaluation */
export type VrfMessage = {
  __typename?: 'VrfMessage';
  /** Position in the ledger of the delegator's account */
  delegatorIndex: Scalars['Int']['output'];
  epochSeed: Scalars['EpochSeed']['output'];
  globalSlot: Scalars['Globalslot']['output'];
};

/** The inputs to a vrf evaluation */
export type VrfMessageInput = {
  /** Position in the ledger of the delegator's account */
  delegatorIndex: Scalars['Int']['input'];
  /** Formatted with base58check */
  epochSeed: Scalars['String']['input'];
  globalSlot: Scalars['UInt32']['input'];
};

/** The amount of stake delegated, used to determine the threshold for a vrf evaluation winning a slot */
export type VrfThreshold = {
  __typename?: 'VrfThreshold';
  /**
   * The amount of stake delegated to the vrf evaluator by the delegating account.
   * This should match the amount in the epoch's staking ledger, which may be
   * different to the amount in the current ledger.
   */
  delegatedStake: Scalars['Balance']['output'];
  /** The total amount of stake across all accounts in the epoch's staking ledger. */
  totalStake: Scalars['Amount']['output'];
};

/** The amount of stake delegated, used to determine the threshold for a vrf evaluation producing a block */
export type VrfThresholdInput = {
  /**
   * The amount of stake delegated to the vrf evaluator by the delegating account.
   * This should match the amount in the epoch's staking ledger, which may be
   * different to the amount in the current ledger.
   */
  delegatedStake: Scalars['UInt64']['input'];
  /** The total amount of stake across all accounts in the epoch's staking ledger. */
  totalStake: Scalars['UInt64']['input'];
};

/** Transition from a source ledger to a target ledger with some fee excess and increase in supply */
export type WorkDescription = {
  __typename?: 'WorkDescription';
  /** Total transaction fee that is not accounted for in the transition from source ledger to target ledger */
  feeExcess: SignedFee;
  /** Base58Check-encoded hash of the source first-pass ledger */
  sourceFirstPassLedgerHash: Scalars['LedgerHash']['output'];
  /** Base58Check-encoded hash of the source second-pass ledger */
  sourceSecondPassLedgerHash: Scalars['LedgerHash']['output'];
  /** Increase/Decrease in total supply */
  supplyChange: SignedFee;
  /**
   * Increase in total supply
   * @deprecated Use supplyChange
   */
  supplyIncrease: Scalars['Amount']['output'];
  /** Base58Check-encoded hash of the target first-pass ledger */
  targetFirstPassLedgerHash: Scalars['LedgerHash']['output'];
  /** Base58Check-encoded hash of the target second-pass ledger */
  targetSecondPassLedgerHash: Scalars['LedgerHash']['output'];
  /** Unique identifier for a snark work */
  workId: Scalars['Int']['output'];
};

/** An account update in a zkApp transaction */
export type ZkappAccountUpdate = {
  __typename?: 'ZkappAccountUpdate';
  authorization: Control;
  body: AccountUpdateBody;
};

/** An account update in a zkApp transaction */
export type ZkappAccountUpdateInput = {
  authorization: ControlInput;
  body: AccountUpdateBodyInput;
};

export type ZkappCommand = {
  __typename?: 'ZkappCommand';
  accountUpdates: Array<ZkappAccountUpdate>;
  feePayer: ZkappFeePayer;
  memo: Scalars['Memo']['output'];
};

export type ZkappCommandFailureReason = {
  __typename?: 'ZkappCommandFailureReason';
  /** Failure reason for the account update or any nested zkapp command */
  failures: Array<Scalars['TransactionStatusFailure']['output']>;
  /** List index of the account update that failed */
  index?: Maybe<Scalars['Index']['output']>;
};

export type ZkappCommandInput = {
  accountUpdates: Array<ZkappAccountUpdateInput>;
  feePayer: ZkappFeePayerInput;
  memo: Scalars['Memo']['input'];
};

export type ZkappCommandResult = {
  __typename?: 'ZkappCommandResult';
  /** The reason for the zkApp transaction failure; null means success or the status is unknown */
  failureReason?: Maybe<Array<Maybe<ZkappCommandFailureReason>>>;
  /** A cryptographic hash of the zkApp command */
  hash: Scalars['TransactionHash']['output'];
  /** A Base64 string representing the zkApp command */
  id: Scalars['TransactionId']['output'];
  /** zkApp command representing the transaction */
  zkappCommand: ZkappCommand;
};

export type ZkappFeePayer = {
  __typename?: 'ZkappFeePayer';
  authorization: Scalars['Signature']['output'];
  body: FeePayerBody;
};

export type ZkappFeePayerInput = {
  authorization: Scalars['Signature']['input'];
  body: FeePayerBodyInput;
};

export type EpochLedger = {
  __typename?: 'epochLedger';
  hash: Scalars['LedgerHash']['output'];
  totalCurrency: Scalars['Amount']['output'];
};

export type Mutation = {
  __typename?: 'mutation';
  /**
   * Add a wallet - this will create a new keypair and store it in the daemon
   * @deprecated use createAccount instead
   */
  addWallet: AddAccountPayload;
  /**
   * Delete the private key for an account that you track
   * @deprecated use deleteAccount instead
   */
  deleteWallet: DeleteAccountPayload;
  /** Send a zkApp (for internal testing purposes) */
  internalSendZkapp: SendZkappPayload;
  /**
   * Lock an unlocked account to prevent transaction being sent from it
   * @deprecated use lockAccount instead
   */
  lockWallet: LockPayload;
  /** Mock a zkApp transaction, no effect on blockchain */
  mockZkapp: SendZkappPayload;
  /**
   * Reload tracked account information from disk
   * @deprecated use reloadAccounts instead
   */
  reloadWallets: ReloadAccountsPayload;
  /** Change your delegate by sending a transaction */
  sendDelegation: SendDelegationPayload;
  /** Send a payment */
  sendPayment: SendPaymentPayload;
  /** Send a transaction in Rosetta format */
  sendRosettaTransaction: SendRosettaTransactionPayload;
  /** Send a series of test payments */
  sendTestPayments: Scalars['Int']['output'];
  /** Send a zkApp transaction */
  sendZkapp: SendZkappPayload;
  /**
   * Allow transactions to be sent from the unlocked account
   * @deprecated use unlockAccount instead
   */
  unlockWallet: UnlockPayload;
};


export type MutationAddWalletArgs = {
  input: AddAccountInput;
};


export type MutationDeleteWalletArgs = {
  input: DeleteAccountInput;
};


export type MutationInternalSendZkappArgs = {
  zkappCommand: Scalars['SendTestZkappInput']['input'];
};


export type MutationLockWalletArgs = {
  input: LockInput;
};


export type MutationMockZkappArgs = {
  input: SendZkappInput;
};


export type MutationSendDelegationArgs = {
  input: SendDelegationInput;
  signature?: InputMaybe<SignatureInput>;
};


export type MutationSendPaymentArgs = {
  input: SendPaymentInput;
  signature?: InputMaybe<SignatureInput>;
};


export type MutationSendRosettaTransactionArgs = {
  input: Scalars['RosettaTransaction']['input'];
};


export type MutationSendTestPaymentsArgs = {
  amount: Scalars['UInt64']['input'];
  fee: Scalars['UInt64']['input'];
  receiver: Scalars['PublicKey']['input'];
  repeat_count: Scalars['UInt32']['input'];
  repeat_delay_ms: Scalars['UInt32']['input'];
  senders: Array<Scalars['PrivateKey']['input']>;
};


export type MutationSendZkappArgs = {
  input: SendZkappInput;
};


export type MutationUnlockWalletArgs = {
  input: UnlockInput;
};

export type ProtocolStateProof = {
  __typename?: 'protocolStateProof';
  /** Base-64 encoded proof */
  base64?: Maybe<Scalars['PrecomputedBlockProof']['output']>;
  /** JSON-encoded proof */
  json?: Maybe<Scalars['JSON']['output']>;
};

export type Query = {
  __typename?: 'query';
  /** Find any account via a public key and token */
  account?: Maybe<Account>;
  /** Find all accounts for a public key */
  accounts: Array<Account>;
  actions: Array<Maybe<ActionOutput>>;
  /**
   * Retrieve a list of blocks from transition frontier's root to the current best
   * tip. Returns an error if the system is bootstrapping.
   */
  bestChain?: Maybe<Array<Block>>;
  /** Retrieve a block with the given state hash or height, if contained in the transition frontier. */
  block: Block;
  /** The pickles verification key for the protocol state proof */
  blockchainVerificationKey: Scalars['JSON']['output'];
  /**
   * Check a vrf evaluation commitment. This can be used to check vrf evaluations
   * without needing to reveal the private key, in the format returned by evaluateVrf
   */
  checkVrf: VrfEvaluation;
  /** Get running daemon status */
  daemonStatus: DaemonStatus;
  /**
   * Evaluate a vrf for the given public key. This includes a witness which may be
   * verified without access to the private key for this vrf evaluation.
   */
  evaluateVrf: VrfEvaluation;
  /** Archiven node queries */
  events: Array<Maybe<EventOutput>>;
  /** Get the genesis block */
  genesisBlock: Block;
  /** The constants used to determine the configuration of the genesis block and all of its transitive dependencies */
  genesisConstants: GenesisConstants;
  /** List of peers that the daemon is currently connected to */
  getPeers: Array<Peer>;
  /** List of peers that the daemon first used to connect to the network */
  initialPeers: Array<Scalars['String']['output']>;
  /**
   * Wallets for which the daemon knows the private key
   * @deprecated use trackedAccounts instead
   */
  ownedWallets: Array<Account>;
  /** List of snark works that are yet to be done */
  pendingSnarkWork: Array<PendingSnarkWork>;
  /**
   * Retrieve all the scheduled user commands for a specified sender that the
   * current daemon sees in its transaction pool. All scheduled commands are
   * queried if no sender is specified
   */
  pooledUserCommands: Array<UserCommand>;
  /**
   * Retrieve all the scheduled zkApp commands for a specified sender that the
   * current daemon sees in its transaction pool. All scheduled commands are
   * queried if no sender is specified
   */
  pooledZkappCommands: Array<ZkappCommandResult>;
  /** List of completed snark works that have the lowest fee so far */
  snarkPool: Array<CompletedWork>;
  /** Network sync status */
  syncStatus: SyncStatus;
  /** The time offset in seconds used to convert real times into blockchain times */
  timeOffset: Scalars['Int']['output'];
  /** Find all accounts for a token ID */
  tokenAccounts: Array<Account>;
  /** Find the account that owns a given token */
  tokenOwner?: Maybe<Account>;
  /** Get the status of a transaction */
  transactionStatus: TransactionStatus;
  /** Trust status for an IPv4 or IPv6 address */
  trustStatus?: Maybe<Array<TrustStatusPayload>>;
  /** IP address and trust status for all peers */
  trustStatusAll: Array<TrustStatusPayload>;
  /** Validate the format and signature of a payment */
  validatePayment: Scalars['Boolean']['output'];
  /** The version of the node (git commit hash) */
  version?: Maybe<Scalars['String']['output']>;
  /**
   * Find any wallet via a public key
   * @deprecated use account instead
   */
  wallet?: Maybe<Account>;
};


export type QueryAccountArgs = {
  publicKey: Scalars['PublicKey']['input'];
  token?: InputMaybe<Scalars['TokenId']['input']>;
};


export type QueryAccountsArgs = {
  publicKey: Scalars['PublicKey']['input'];
};


export type QueryActionsArgs = {
  input: ActionFilterOptionsInput;
};


export type QueryBestChainArgs = {
  maxLength?: InputMaybe<Scalars['Int']['input']>;
};


export type QueryBlockArgs = {
  height?: InputMaybe<Scalars['Int']['input']>;
  stateHash?: InputMaybe<Scalars['String']['input']>;
};


export type QueryCheckVrfArgs = {
  input: VrfEvaluationInput;
};


export type QueryEvaluateVrfArgs = {
  message: VrfMessageInput;
  publicKey: Scalars['PublicKey']['input'];
  vrfThreshold?: InputMaybe<VrfThresholdInput>;
};


export type QueryEventsArgs = {
  input: EventFilterOptionsInput;
};


export type QueryPooledUserCommandsArgs = {
  hashes?: InputMaybe<Array<Scalars['String']['input']>>;
  ids?: InputMaybe<Array<Scalars['ID']['input']>>;
  publicKey?: InputMaybe<Scalars['PublicKey']['input']>;
};


export type QueryPooledZkappCommandsArgs = {
  hashes?: InputMaybe<Array<Scalars['String']['input']>>;
  ids?: InputMaybe<Array<Scalars['ID']['input']>>;
  publicKey?: InputMaybe<Scalars['PublicKey']['input']>;
};


export type QueryTokenAccountsArgs = {
  tokenId: Scalars['TokenId']['input'];
};


export type QueryTokenOwnerArgs = {
  tokenId: Scalars['TokenId']['input'];
};


export type QueryTransactionStatusArgs = {
  payment?: InputMaybe<Scalars['ID']['input']>;
  zkappTransaction?: InputMaybe<Scalars['ID']['input']>;
};


export type QueryTrustStatusArgs = {
  ipAddress: Scalars['String']['input'];
};


export type QueryValidatePaymentArgs = {
  input: SendPaymentInput;
  signature?: InputMaybe<SignatureInput>;
};


export type QueryWalletArgs = {
  publicKey: Scalars['PublicKey']['input'];
};

export enum Sign {
  Minus = 'MINUS',
  Plus = 'PLUS'
}

export type Subscription = {
  __typename?: 'subscription';
  /** Event that triggers when the best tip changes in a way that is not a trivial extension of the existing one */
  chainReorganization: ChainReorganizationStatus;
  /**
   * Event that triggers when a new block is created that either contains a
   * transaction with the specified public key, or was produced by it. If no public
   * key is provided, then the event will trigger for every new block received
   */
  newBlock: Block;
  /** Event that triggers when the network sync status changes */
  newSyncUpdate: SyncStatus;
};


export type SubscriptionNewBlockArgs = {
  publicKey?: InputMaybe<Scalars['PublicKey']['input']>;
};



export type ResolverTypeWrapper<T> = Promise<T> | T;


export type ResolverWithResolve<TResult, TParent, TContext, TArgs> = {
  resolve: ResolverFn<TResult, TParent, TContext, TArgs>;
};
export type Resolver<TResult, TParent = {}, TContext = {}, TArgs = {}> = ResolverFn<TResult, TParent, TContext, TArgs> | ResolverWithResolve<TResult, TParent, TContext, TArgs>;

export type ResolverFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => Promise<TResult> | TResult;

export type SubscriptionSubscribeFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => AsyncIterable<TResult> | Promise<AsyncIterable<TResult>>;

export type SubscriptionResolveFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

export interface SubscriptionSubscriberObject<TResult, TKey extends string, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<{ [key in TKey]: TResult }, TParent, TContext, TArgs>;
  resolve?: SubscriptionResolveFn<TResult, { [key in TKey]: TResult }, TContext, TArgs>;
}

export interface SubscriptionResolverObject<TResult, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<any, TParent, TContext, TArgs>;
  resolve: SubscriptionResolveFn<TResult, any, TContext, TArgs>;
}

export type SubscriptionObject<TResult, TKey extends string, TParent, TContext, TArgs> =
  | SubscriptionSubscriberObject<TResult, TKey, TParent, TContext, TArgs>
  | SubscriptionResolverObject<TResult, TParent, TContext, TArgs>;

export type SubscriptionResolver<TResult, TKey extends string, TParent = {}, TContext = {}, TArgs = {}> =
  | ((...args: any[]) => SubscriptionObject<TResult, TKey, TParent, TContext, TArgs>)
  | SubscriptionObject<TResult, TKey, TParent, TContext, TArgs>;

export type TypeResolveFn<TTypes, TParent = {}, TContext = {}> = (
  parent: TParent,
  context: TContext,
  info: GraphQLResolveInfo
) => Maybe<TTypes> | Promise<Maybe<TTypes>>;

export type IsTypeOfResolverFn<T = {}, TContext = {}> = (obj: T, context: TContext, info: GraphQLResolveInfo) => boolean | Promise<boolean>;

export type NextResolverFn<T> = () => Promise<T>;

export type DirectiveResolverFn<TResult = {}, TParent = {}, TContext = {}, TArgs = {}> = (
  next: NextResolverFn<TResult>,
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;


/** Mapping of interface types */
export type ResolversInterfaceTypes<RefType extends Record<string, unknown>> = {
  UserCommand: ( UserCommandDelegation ) | ( UserCommandPayment );
};

/** Mapping between all available schema types and the resolvers types */
export type ResolversTypes = {
  Account: ResolverTypeWrapper<Account>;
  AccountAuthRequired: AccountAuthRequired;
  AccountNonce: ResolverTypeWrapper<Scalars['AccountNonce']['output']>;
  AccountPermissions: ResolverTypeWrapper<AccountPermissions>;
  AccountPrecondition: ResolverTypeWrapper<AccountPrecondition>;
  AccountPreconditionInput: AccountPreconditionInput;
  AccountTiming: ResolverTypeWrapper<AccountTiming>;
  AccountUpdateBody: ResolverTypeWrapper<AccountUpdateBody>;
  AccountUpdateBodyInput: AccountUpdateBodyInput;
  AccountUpdateModification: ResolverTypeWrapper<AccountUpdateModification>;
  AccountUpdateModificationInput: AccountUpdateModificationInput;
  AccountVerificationKeyWithHash: ResolverTypeWrapper<AccountVerificationKeyWithHash>;
  Action: ResolverTypeWrapper<Scalars['Action']['output']>;
  ActionData: ResolverTypeWrapper<ActionData>;
  ActionFilterOptionsInput: ActionFilterOptionsInput;
  ActionOutput: ResolverTypeWrapper<ActionOutput>;
  ActionStates: ResolverTypeWrapper<ActionStates>;
  AddAccountInput: AddAccountInput;
  AddAccountPayload: ResolverTypeWrapper<AddAccountPayload>;
  AddrsAndPorts: ResolverTypeWrapper<AddrsAndPorts>;
  Amount: ResolverTypeWrapper<Scalars['Amount']['output']>;
  AnnotatedBalance: ResolverTypeWrapper<AnnotatedBalance>;
  Applied: ResolverTypeWrapper<Applied>;
  AuthRequired: ResolverTypeWrapper<Scalars['AuthRequired']['output']>;
  AuthorizationKindStructured: ResolverTypeWrapper<AuthorizationKindStructured>;
  AuthorizationKindStructuredInput: AuthorizationKindStructuredInput;
  Balance: ResolverTypeWrapper<Scalars['Balance']['output']>;
  BalanceChange: ResolverTypeWrapper<BalanceChange>;
  BalanceChangeInput: BalanceChangeInput;
  BalanceInterval: ResolverTypeWrapper<BalanceInterval>;
  BalanceIntervalInput: BalanceIntervalInput;
  Block: ResolverTypeWrapper<Block>;
  BlockInfo: ResolverTypeWrapper<BlockInfo>;
  BlockProducerTimings: ResolverTypeWrapper<BlockProducerTimings>;
  BlockStatusFilter: BlockStatusFilter;
  BlockTime: ResolverTypeWrapper<Scalars['BlockTime']['output']>;
  BlockchainState: ResolverTypeWrapper<BlockchainState>;
  BodyReference: ResolverTypeWrapper<Scalars['BodyReference']['output']>;
  Boolean: ResolverTypeWrapper<Scalars['Boolean']['output']>;
  ChainHash: ResolverTypeWrapper<Scalars['ChainHash']['output']>;
  ChainReorganizationStatus: ChainReorganizationStatus;
  CompletedWork: ResolverTypeWrapper<CompletedWork>;
  ConsensusConfiguration: ResolverTypeWrapper<ConsensusConfiguration>;
  ConsensusState: ResolverTypeWrapper<ConsensusState>;
  ConsensusTime: ResolverTypeWrapper<ConsensusTime>;
  ConsensusTimeGlobalSlot: ResolverTypeWrapper<ConsensusTimeGlobalSlot>;
  Control: ResolverTypeWrapper<Control>;
  ControlInput: ControlInput;
  CreateHDAccountInput: CreateHdAccountInput;
  CurrencyAmount: ResolverTypeWrapper<Scalars['CurrencyAmount']['output']>;
  CurrencyAmountInterval: ResolverTypeWrapper<CurrencyAmountInterval>;
  CurrencyAmountIntervalInput: CurrencyAmountIntervalInput;
  DaemonStatus: ResolverTypeWrapper<DaemonStatus>;
  DeleteAccountInput: DeleteAccountInput;
  DeleteAccountPayload: ResolverTypeWrapper<DeleteAccountPayload>;
  Epoch: ResolverTypeWrapper<Scalars['Epoch']['output']>;
  EpochDataPrecondition: ResolverTypeWrapper<EpochDataPrecondition>;
  EpochDataPreconditionInput: EpochDataPreconditionInput;
  EpochLedgerPrecondition: ResolverTypeWrapper<EpochLedgerPrecondition>;
  EpochLedgerPreconditionInput: EpochLedgerPreconditionInput;
  EpochSeed: ResolverTypeWrapper<Scalars['EpochSeed']['output']>;
  EventData: ResolverTypeWrapper<EventData>;
  EventFilterOptionsInput: EventFilterOptionsInput;
  EventOutput: ResolverTypeWrapper<EventOutput>;
  ExportLogsPayload: ResolverTypeWrapper<ExportLogsPayload>;
  ExtensionalBlock: ResolverTypeWrapper<Scalars['ExtensionalBlock']['output']>;
  Fee: ResolverTypeWrapper<Scalars['Fee']['output']>;
  FeePayerBody: ResolverTypeWrapper<FeePayerBody>;
  FeePayerBodyInput: FeePayerBodyInput;
  FeeTransfer: ResolverTypeWrapper<FeeTransfer>;
  FeeTransferType: ResolverTypeWrapper<Scalars['FeeTransferType']['output']>;
  Field: ResolverTypeWrapper<Scalars['Field']['output']>;
  FieldElem: ResolverTypeWrapper<Scalars['FieldElem']['output']>;
  Float: ResolverTypeWrapper<Scalars['Float']['output']>;
  GenesisConstants: ResolverTypeWrapper<GenesisConstants>;
  GlobalSlot: ResolverTypeWrapper<Scalars['GlobalSlot']['output']>;
  GlobalSlotInterval: ResolverTypeWrapper<GlobalSlotInterval>;
  GlobalSlotIntervalInput: GlobalSlotIntervalInput;
  Globalslot: ResolverTypeWrapper<Scalars['Globalslot']['output']>;
  Histogram: ResolverTypeWrapper<Histogram>;
  Histograms: ResolverTypeWrapper<Histograms>;
  ID: ResolverTypeWrapper<Scalars['ID']['output']>;
  ImportAccountPayload: ResolverTypeWrapper<ImportAccountPayload>;
  Index: ResolverTypeWrapper<Scalars['Index']['output']>;
  InetAddr: ResolverTypeWrapper<Scalars['InetAddr']['output']>;
  Int: ResolverTypeWrapper<Scalars['Int']['output']>;
  Interval: ResolverTypeWrapper<Interval>;
  JSON: ResolverTypeWrapper<Scalars['JSON']['output']>;
  LedgerHash: ResolverTypeWrapper<Scalars['LedgerHash']['output']>;
  Length: ResolverTypeWrapper<Scalars['Length']['output']>;
  LengthInterval: ResolverTypeWrapper<LengthInterval>;
  LengthIntervalInput: LengthIntervalInput;
  LockInput: LockInput;
  LockPayload: ResolverTypeWrapper<LockPayload>;
  MayUseToken: ResolverTypeWrapper<MayUseToken>;
  MayUseTokenInput: MayUseTokenInput;
  Memo: ResolverTypeWrapper<Scalars['Memo']['output']>;
  MerklePathElement: ResolverTypeWrapper<MerklePathElement>;
  Metrics: ResolverTypeWrapper<Metrics>;
  NetworkPeer: NetworkPeer;
  NetworkPeerPayload: ResolverTypeWrapper<NetworkPeerPayload>;
  NetworkPrecondition: ResolverTypeWrapper<NetworkPrecondition>;
  NetworkPreconditionInput: NetworkPreconditionInput;
  NextEpochData: ResolverTypeWrapper<NextEpochData>;
  NonceInterval: ResolverTypeWrapper<NonceInterval>;
  NonceIntervalInput: NonceIntervalInput;
  Peer: ResolverTypeWrapper<Peer>;
  PendingCoinbaseAuxHash: ResolverTypeWrapper<Scalars['PendingCoinbaseAuxHash']['output']>;
  PendingCoinbaseHash: ResolverTypeWrapper<Scalars['PendingCoinbaseHash']['output']>;
  PendingSnarkWork: ResolverTypeWrapper<PendingSnarkWork>;
  Permissions: ResolverTypeWrapper<Permissions>;
  PermissionsInput: PermissionsInput;
  PrecomputedBlock: ResolverTypeWrapper<Scalars['PrecomputedBlock']['output']>;
  PrecomputedBlockProof: ResolverTypeWrapper<Scalars['PrecomputedBlockProof']['output']>;
  Preconditions: ResolverTypeWrapper<Preconditions>;
  PreconditionsInput: PreconditionsInput;
  PrivateKey: ResolverTypeWrapper<Scalars['PrivateKey']['output']>;
  ProtocolState: ResolverTypeWrapper<ProtocolState>;
  PublicKey: ResolverTypeWrapper<Scalars['PublicKey']['output']>;
  ReloadAccountsPayload: ResolverTypeWrapper<ReloadAccountsPayload>;
  RosettaTransaction: ResolverTypeWrapper<Scalars['RosettaTransaction']['output']>;
  RpcPair: ResolverTypeWrapper<RpcPair>;
  RpcTimings: ResolverTypeWrapper<RpcTimings>;
  SendDelegationInput: SendDelegationInput;
  SendDelegationPayload: ResolverTypeWrapper<SendDelegationPayload>;
  SendPaymentInput: SendPaymentInput;
  SendPaymentPayload: ResolverTypeWrapper<SendPaymentPayload>;
  SendRosettaTransactionPayload: ResolverTypeWrapper<SendRosettaTransactionPayload>;
  SendTestZkappInput: ResolverTypeWrapper<Scalars['SendTestZkappInput']['output']>;
  SendZkappInput: SendZkappInput;
  SendZkappPayload: ResolverTypeWrapper<SendZkappPayload>;
  SetCoinbaseReceiverInput: SetCoinbaseReceiverInput;
  SetCoinbaseReceiverPayload: ResolverTypeWrapper<SetCoinbaseReceiverPayload>;
  SetConnectionGatingConfigInput: SetConnectionGatingConfigInput;
  SetConnectionGatingConfigPayload: ResolverTypeWrapper<SetConnectionGatingConfigPayload>;
  SetSnarkWorkFee: SetSnarkWorkFee;
  SetSnarkWorkFeePayload: ResolverTypeWrapper<SetSnarkWorkFeePayload>;
  SetSnarkWorkerInput: SetSnarkWorkerInput;
  SetSnarkWorkerPayload: ResolverTypeWrapper<SetSnarkWorkerPayload>;
  Sign: ResolverTypeWrapper<Scalars['Sign']['output']>;
  Signature: ResolverTypeWrapper<Scalars['Signature']['output']>;
  SignatureInput: SignatureInput;
  SignedFee: ResolverTypeWrapper<SignedFee>;
  Slot: ResolverTypeWrapper<Scalars['Slot']['output']>;
  SnarkWorker: ResolverTypeWrapper<SnarkWorker>;
  Span: ResolverTypeWrapper<Scalars['Span']['output']>;
  StagedLedgerAuxHash: ResolverTypeWrapper<Scalars['StagedLedgerAuxHash']['output']>;
  StakingEpochData: ResolverTypeWrapper<StakingEpochData>;
  StateHash: ResolverTypeWrapper<Scalars['StateHash']['output']>;
  StateHashAsDecimal: ResolverTypeWrapper<Scalars['StateHashAsDecimal']['output']>;
  String: ResolverTypeWrapper<Scalars['String']['output']>;
  SyncStatus: SyncStatus;
  TarFile: ResolverTypeWrapper<TarFile>;
  Time: ResolverTypeWrapper<Scalars['Time']['output']>;
  Timing: ResolverTypeWrapper<Timing>;
  TimingInput: TimingInput;
  TokenId: ResolverTypeWrapper<Scalars['TokenId']['output']>;
  TransactionHash: ResolverTypeWrapper<Scalars['TransactionHash']['output']>;
  TransactionId: ResolverTypeWrapper<Scalars['TransactionId']['output']>;
  TransactionInfo: ResolverTypeWrapper<TransactionInfo>;
  TransactionStatus: TransactionStatus;
  TransactionStatusFailure: ResolverTypeWrapper<Scalars['TransactionStatusFailure']['output']>;
  Transactions: ResolverTypeWrapper<Transactions>;
  TrustStatusPayload: ResolverTypeWrapper<TrustStatusPayload>;
  UInt32: ResolverTypeWrapper<Scalars['UInt32']['output']>;
  UInt64: ResolverTypeWrapper<Scalars['UInt64']['output']>;
  UnlockInput: UnlockInput;
  UnlockPayload: ResolverTypeWrapper<UnlockPayload>;
  UserCommand: ResolverTypeWrapper<ResolversInterfaceTypes<ResolversTypes>['UserCommand']>;
  UserCommandDelegation: ResolverTypeWrapper<UserCommandDelegation>;
  UserCommandKind: ResolverTypeWrapper<Scalars['UserCommandKind']['output']>;
  UserCommandPayment: ResolverTypeWrapper<UserCommandPayment>;
  VerificationKey: ResolverTypeWrapper<Scalars['VerificationKey']['output']>;
  VerificationKeyHash: ResolverTypeWrapper<Scalars['VerificationKeyHash']['output']>;
  VerificationKeyWithHash: ResolverTypeWrapper<VerificationKeyWithHash>;
  VerificationKeyWithHashInput: VerificationKeyWithHashInput;
  VrfEvaluation: ResolverTypeWrapper<VrfEvaluation>;
  VrfEvaluationInput: VrfEvaluationInput;
  VrfMessage: ResolverTypeWrapper<VrfMessage>;
  VrfMessageInput: VrfMessageInput;
  VrfOutputTruncated: ResolverTypeWrapper<Scalars['VrfOutputTruncated']['output']>;
  VrfScalar: ResolverTypeWrapper<Scalars['VrfScalar']['output']>;
  VrfThreshold: ResolverTypeWrapper<VrfThreshold>;
  VrfThresholdInput: VrfThresholdInput;
  WorkDescription: ResolverTypeWrapper<WorkDescription>;
  ZkappAccountUpdate: ResolverTypeWrapper<ZkappAccountUpdate>;
  ZkappAccountUpdateInput: ZkappAccountUpdateInput;
  ZkappCommand: ResolverTypeWrapper<ZkappCommand>;
  ZkappCommandFailureReason: ResolverTypeWrapper<ZkappCommandFailureReason>;
  ZkappCommandInput: ZkappCommandInput;
  ZkappCommandResult: ResolverTypeWrapper<ZkappCommandResult>;
  ZkappFeePayer: ResolverTypeWrapper<ZkappFeePayer>;
  ZkappFeePayerInput: ZkappFeePayerInput;
  ZkappProof: ResolverTypeWrapper<Scalars['ZkappProof']['output']>;
  epochLedger: ResolverTypeWrapper<EpochLedger>;
  mutation: ResolverTypeWrapper<{}>;
  protocolStateProof: ResolverTypeWrapper<ProtocolStateProof>;
  query: ResolverTypeWrapper<{}>;
  sign: Sign;
  subscription: ResolverTypeWrapper<{}>;
};

/** Mapping between all available schema types and the resolvers parents */
export type ResolversParentTypes = {
  Account: Account;
  AccountNonce: Scalars['AccountNonce']['output'];
  AccountPermissions: AccountPermissions;
  AccountPrecondition: AccountPrecondition;
  AccountPreconditionInput: AccountPreconditionInput;
  AccountTiming: AccountTiming;
  AccountUpdateBody: AccountUpdateBody;
  AccountUpdateBodyInput: AccountUpdateBodyInput;
  AccountUpdateModification: AccountUpdateModification;
  AccountUpdateModificationInput: AccountUpdateModificationInput;
  AccountVerificationKeyWithHash: AccountVerificationKeyWithHash;
  Action: Scalars['Action']['output'];
  ActionData: ActionData;
  ActionFilterOptionsInput: ActionFilterOptionsInput;
  ActionOutput: ActionOutput;
  ActionStates: ActionStates;
  AddAccountInput: AddAccountInput;
  AddAccountPayload: AddAccountPayload;
  AddrsAndPorts: AddrsAndPorts;
  Amount: Scalars['Amount']['output'];
  AnnotatedBalance: AnnotatedBalance;
  Applied: Applied;
  AuthRequired: Scalars['AuthRequired']['output'];
  AuthorizationKindStructured: AuthorizationKindStructured;
  AuthorizationKindStructuredInput: AuthorizationKindStructuredInput;
  Balance: Scalars['Balance']['output'];
  BalanceChange: BalanceChange;
  BalanceChangeInput: BalanceChangeInput;
  BalanceInterval: BalanceInterval;
  BalanceIntervalInput: BalanceIntervalInput;
  Block: Block;
  BlockInfo: BlockInfo;
  BlockProducerTimings: BlockProducerTimings;
  BlockTime: Scalars['BlockTime']['output'];
  BlockchainState: BlockchainState;
  BodyReference: Scalars['BodyReference']['output'];
  Boolean: Scalars['Boolean']['output'];
  ChainHash: Scalars['ChainHash']['output'];
  CompletedWork: CompletedWork;
  ConsensusConfiguration: ConsensusConfiguration;
  ConsensusState: ConsensusState;
  ConsensusTime: ConsensusTime;
  ConsensusTimeGlobalSlot: ConsensusTimeGlobalSlot;
  Control: Control;
  ControlInput: ControlInput;
  CreateHDAccountInput: CreateHdAccountInput;
  CurrencyAmount: Scalars['CurrencyAmount']['output'];
  CurrencyAmountInterval: CurrencyAmountInterval;
  CurrencyAmountIntervalInput: CurrencyAmountIntervalInput;
  DaemonStatus: DaemonStatus;
  DeleteAccountInput: DeleteAccountInput;
  DeleteAccountPayload: DeleteAccountPayload;
  Epoch: Scalars['Epoch']['output'];
  EpochDataPrecondition: EpochDataPrecondition;
  EpochDataPreconditionInput: EpochDataPreconditionInput;
  EpochLedgerPrecondition: EpochLedgerPrecondition;
  EpochLedgerPreconditionInput: EpochLedgerPreconditionInput;
  EpochSeed: Scalars['EpochSeed']['output'];
  EventData: EventData;
  EventFilterOptionsInput: EventFilterOptionsInput;
  EventOutput: EventOutput;
  ExportLogsPayload: ExportLogsPayload;
  ExtensionalBlock: Scalars['ExtensionalBlock']['output'];
  Fee: Scalars['Fee']['output'];
  FeePayerBody: FeePayerBody;
  FeePayerBodyInput: FeePayerBodyInput;
  FeeTransfer: FeeTransfer;
  FeeTransferType: Scalars['FeeTransferType']['output'];
  Field: Scalars['Field']['output'];
  FieldElem: Scalars['FieldElem']['output'];
  Float: Scalars['Float']['output'];
  GenesisConstants: GenesisConstants;
  GlobalSlot: Scalars['GlobalSlot']['output'];
  GlobalSlotInterval: GlobalSlotInterval;
  GlobalSlotIntervalInput: GlobalSlotIntervalInput;
  Globalslot: Scalars['Globalslot']['output'];
  Histogram: Histogram;
  Histograms: Histograms;
  ID: Scalars['ID']['output'];
  ImportAccountPayload: ImportAccountPayload;
  Index: Scalars['Index']['output'];
  InetAddr: Scalars['InetAddr']['output'];
  Int: Scalars['Int']['output'];
  Interval: Interval;
  JSON: Scalars['JSON']['output'];
  LedgerHash: Scalars['LedgerHash']['output'];
  Length: Scalars['Length']['output'];
  LengthInterval: LengthInterval;
  LengthIntervalInput: LengthIntervalInput;
  LockInput: LockInput;
  LockPayload: LockPayload;
  MayUseToken: MayUseToken;
  MayUseTokenInput: MayUseTokenInput;
  Memo: Scalars['Memo']['output'];
  MerklePathElement: MerklePathElement;
  Metrics: Metrics;
  NetworkPeer: NetworkPeer;
  NetworkPeerPayload: NetworkPeerPayload;
  NetworkPrecondition: NetworkPrecondition;
  NetworkPreconditionInput: NetworkPreconditionInput;
  NextEpochData: NextEpochData;
  NonceInterval: NonceInterval;
  NonceIntervalInput: NonceIntervalInput;
  Peer: Peer;
  PendingCoinbaseAuxHash: Scalars['PendingCoinbaseAuxHash']['output'];
  PendingCoinbaseHash: Scalars['PendingCoinbaseHash']['output'];
  PendingSnarkWork: PendingSnarkWork;
  Permissions: Permissions;
  PermissionsInput: PermissionsInput;
  PrecomputedBlock: Scalars['PrecomputedBlock']['output'];
  PrecomputedBlockProof: Scalars['PrecomputedBlockProof']['output'];
  Preconditions: Preconditions;
  PreconditionsInput: PreconditionsInput;
  PrivateKey: Scalars['PrivateKey']['output'];
  ProtocolState: ProtocolState;
  PublicKey: Scalars['PublicKey']['output'];
  ReloadAccountsPayload: ReloadAccountsPayload;
  RosettaTransaction: Scalars['RosettaTransaction']['output'];
  RpcPair: RpcPair;
  RpcTimings: RpcTimings;
  SendDelegationInput: SendDelegationInput;
  SendDelegationPayload: SendDelegationPayload;
  SendPaymentInput: SendPaymentInput;
  SendPaymentPayload: SendPaymentPayload;
  SendRosettaTransactionPayload: SendRosettaTransactionPayload;
  SendTestZkappInput: Scalars['SendTestZkappInput']['output'];
  SendZkappInput: SendZkappInput;
  SendZkappPayload: SendZkappPayload;
  SetCoinbaseReceiverInput: SetCoinbaseReceiverInput;
  SetCoinbaseReceiverPayload: SetCoinbaseReceiverPayload;
  SetConnectionGatingConfigInput: SetConnectionGatingConfigInput;
  SetConnectionGatingConfigPayload: SetConnectionGatingConfigPayload;
  SetSnarkWorkFee: SetSnarkWorkFee;
  SetSnarkWorkFeePayload: SetSnarkWorkFeePayload;
  SetSnarkWorkerInput: SetSnarkWorkerInput;
  SetSnarkWorkerPayload: SetSnarkWorkerPayload;
  Sign: Scalars['Sign']['output'];
  Signature: Scalars['Signature']['output'];
  SignatureInput: SignatureInput;
  SignedFee: SignedFee;
  Slot: Scalars['Slot']['output'];
  SnarkWorker: SnarkWorker;
  Span: Scalars['Span']['output'];
  StagedLedgerAuxHash: Scalars['StagedLedgerAuxHash']['output'];
  StakingEpochData: StakingEpochData;
  StateHash: Scalars['StateHash']['output'];
  StateHashAsDecimal: Scalars['StateHashAsDecimal']['output'];
  String: Scalars['String']['output'];
  TarFile: TarFile;
  Time: Scalars['Time']['output'];
  Timing: Timing;
  TimingInput: TimingInput;
  TokenId: Scalars['TokenId']['output'];
  TransactionHash: Scalars['TransactionHash']['output'];
  TransactionId: Scalars['TransactionId']['output'];
  TransactionInfo: TransactionInfo;
  TransactionStatusFailure: Scalars['TransactionStatusFailure']['output'];
  Transactions: Transactions;
  TrustStatusPayload: TrustStatusPayload;
  UInt32: Scalars['UInt32']['output'];
  UInt64: Scalars['UInt64']['output'];
  UnlockInput: UnlockInput;
  UnlockPayload: UnlockPayload;
  UserCommand: ResolversInterfaceTypes<ResolversParentTypes>['UserCommand'];
  UserCommandDelegation: UserCommandDelegation;
  UserCommandKind: Scalars['UserCommandKind']['output'];
  UserCommandPayment: UserCommandPayment;
  VerificationKey: Scalars['VerificationKey']['output'];
  VerificationKeyHash: Scalars['VerificationKeyHash']['output'];
  VerificationKeyWithHash: VerificationKeyWithHash;
  VerificationKeyWithHashInput: VerificationKeyWithHashInput;
  VrfEvaluation: VrfEvaluation;
  VrfEvaluationInput: VrfEvaluationInput;
  VrfMessage: VrfMessage;
  VrfMessageInput: VrfMessageInput;
  VrfOutputTruncated: Scalars['VrfOutputTruncated']['output'];
  VrfScalar: Scalars['VrfScalar']['output'];
  VrfThreshold: VrfThreshold;
  VrfThresholdInput: VrfThresholdInput;
  WorkDescription: WorkDescription;
  ZkappAccountUpdate: ZkappAccountUpdate;
  ZkappAccountUpdateInput: ZkappAccountUpdateInput;
  ZkappCommand: ZkappCommand;
  ZkappCommandFailureReason: ZkappCommandFailureReason;
  ZkappCommandInput: ZkappCommandInput;
  ZkappCommandResult: ZkappCommandResult;
  ZkappFeePayer: ZkappFeePayer;
  ZkappFeePayerInput: ZkappFeePayerInput;
  ZkappProof: Scalars['ZkappProof']['output'];
  epochLedger: EpochLedger;
  mutation: {};
  protocolStateProof: ProtocolStateProof;
  query: {};
  subscription: {};
};

export type AccountResolvers<ContextType = any, ParentType extends ResolversParentTypes['Account'] = ResolversParentTypes['Account']> = {
  actionState?: Resolver<Maybe<Array<ResolversTypes['Action']>>, ParentType, ContextType>;
  balance?: Resolver<ResolversTypes['AnnotatedBalance'], ParentType, ContextType>;
  delegate?: Resolver<Maybe<ResolversTypes['PublicKey']>, ParentType, ContextType>;
  delegateAccount?: Resolver<Maybe<ResolversTypes['Account']>, ParentType, ContextType>;
  delegators?: Resolver<Maybe<Array<ResolversTypes['Account']>>, ParentType, ContextType>;
  epochDelegateAccount?: Resolver<Maybe<ResolversTypes['Account']>, ParentType, ContextType>;
  index?: Resolver<Maybe<ResolversTypes['Int']>, ParentType, ContextType>;
  inferredNonce?: Resolver<Maybe<ResolversTypes['AccountNonce']>, ParentType, ContextType>;
  lastEpochDelegators?: Resolver<Maybe<Array<ResolversTypes['Account']>>, ParentType, ContextType>;
  leafHash?: Resolver<Maybe<ResolversTypes['FieldElem']>, ParentType, ContextType>;
  locked?: Resolver<Maybe<ResolversTypes['Boolean']>, ParentType, ContextType>;
  merklePath?: Resolver<Maybe<Array<ResolversTypes['MerklePathElement']>>, ParentType, ContextType>;
  nonce?: Resolver<Maybe<ResolversTypes['AccountNonce']>, ParentType, ContextType>;
  permissions?: Resolver<Maybe<ResolversTypes['AccountPermissions']>, ParentType, ContextType>;
  privateKeyPath?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  provedState?: Resolver<Maybe<ResolversTypes['Boolean']>, ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  receiptChainHash?: Resolver<Maybe<ResolversTypes['ChainHash']>, ParentType, ContextType>;
  stakingActive?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  timing?: Resolver<ResolversTypes['AccountTiming'], ParentType, ContextType>;
  token?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  tokenId?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  tokenSymbol?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  verificationKey?: Resolver<Maybe<ResolversTypes['AccountVerificationKeyWithHash']>, ParentType, ContextType>;
  votingFor?: Resolver<Maybe<ResolversTypes['ChainHash']>, ParentType, ContextType>;
  zkappState?: Resolver<Maybe<Array<ResolversTypes['FieldElem']>>, ParentType, ContextType>;
  zkappUri?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface AccountNonceScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['AccountNonce'], any> {
  name: 'AccountNonce';
}

export type AccountPermissionsResolvers<ContextType = any, ParentType extends ResolversParentTypes['AccountPermissions'] = ResolversParentTypes['AccountPermissions']> = {
  access?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  editActionState?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  editState?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  incrementNonce?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  receive?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  send?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setDelegate?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setPermissions?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setTiming?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setTokenSymbol?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setVerificationKey?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setVotingFor?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  setZkappUri?: Resolver<ResolversTypes['AccountAuthRequired'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AccountPreconditionResolvers<ContextType = any, ParentType extends ResolversParentTypes['AccountPrecondition'] = ResolversParentTypes['AccountPrecondition']> = {
  actionState?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  balance?: Resolver<Maybe<ResolversTypes['BalanceInterval']>, ParentType, ContextType>;
  delegate?: Resolver<Maybe<ResolversTypes['PublicKey']>, ParentType, ContextType>;
  isNew?: Resolver<Maybe<ResolversTypes['Boolean']>, ParentType, ContextType>;
  nonce?: Resolver<Maybe<ResolversTypes['NonceInterval']>, ParentType, ContextType>;
  provedState?: Resolver<Maybe<ResolversTypes['Boolean']>, ParentType, ContextType>;
  receiptChainHash?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  state?: Resolver<Array<Maybe<ResolversTypes['Field']>>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AccountTimingResolvers<ContextType = any, ParentType extends ResolversParentTypes['AccountTiming'] = ResolversParentTypes['AccountTiming']> = {
  cliffAmount?: Resolver<Maybe<ResolversTypes['Amount']>, ParentType, ContextType>;
  cliffTime?: Resolver<Maybe<ResolversTypes['Globalslot']>, ParentType, ContextType>;
  initialMinimumBalance?: Resolver<Maybe<ResolversTypes['Balance']>, ParentType, ContextType>;
  vestingIncrement?: Resolver<Maybe<ResolversTypes['Amount']>, ParentType, ContextType>;
  vestingPeriod?: Resolver<Maybe<ResolversTypes['Globalslot']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AccountUpdateBodyResolvers<ContextType = any, ParentType extends ResolversParentTypes['AccountUpdateBody'] = ResolversParentTypes['AccountUpdateBody']> = {
  actions?: Resolver<Array<Array<ResolversTypes['Field']>>, ParentType, ContextType>;
  authorizationKind?: Resolver<ResolversTypes['AuthorizationKindStructured'], ParentType, ContextType>;
  balanceChange?: Resolver<ResolversTypes['BalanceChange'], ParentType, ContextType>;
  callData?: Resolver<ResolversTypes['Field'], ParentType, ContextType>;
  callDepth?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  events?: Resolver<Array<Array<ResolversTypes['Field']>>, ParentType, ContextType>;
  implicitAccountCreationFee?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  incrementNonce?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  mayUseToken?: Resolver<ResolversTypes['MayUseToken'], ParentType, ContextType>;
  preconditions?: Resolver<ResolversTypes['Preconditions'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  tokenId?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  update?: Resolver<ResolversTypes['AccountUpdateModification'], ParentType, ContextType>;
  useFullCommitment?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AccountUpdateModificationResolvers<ContextType = any, ParentType extends ResolversParentTypes['AccountUpdateModification'] = ResolversParentTypes['AccountUpdateModification']> = {
  appState?: Resolver<Array<Maybe<ResolversTypes['Field']>>, ParentType, ContextType>;
  delegate?: Resolver<Maybe<ResolversTypes['PublicKey']>, ParentType, ContextType>;
  permissions?: Resolver<Maybe<ResolversTypes['Permissions']>, ParentType, ContextType>;
  timing?: Resolver<Maybe<ResolversTypes['Timing']>, ParentType, ContextType>;
  tokenSymbol?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  verificationKey?: Resolver<Maybe<ResolversTypes['VerificationKeyWithHash']>, ParentType, ContextType>;
  votingFor?: Resolver<Maybe<ResolversTypes['StateHash']>, ParentType, ContextType>;
  zkappUri?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AccountVerificationKeyWithHashResolvers<ContextType = any, ParentType extends ResolversParentTypes['AccountVerificationKeyWithHash'] = ResolversParentTypes['AccountVerificationKeyWithHash']> = {
  hash?: Resolver<ResolversTypes['VerificationKeyHash'], ParentType, ContextType>;
  verificationKey?: Resolver<ResolversTypes['VerificationKey'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface ActionScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Action'], any> {
  name: 'Action';
}

export type ActionDataResolvers<ContextType = any, ParentType extends ResolversParentTypes['ActionData'] = ResolversParentTypes['ActionData']> = {
  accountUpdateId?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  data?: Resolver<Array<Maybe<ResolversTypes['String']>>, ParentType, ContextType>;
  transactionInfo?: Resolver<Maybe<ResolversTypes['TransactionInfo']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ActionOutputResolvers<ContextType = any, ParentType extends ResolversParentTypes['ActionOutput'] = ResolversParentTypes['ActionOutput']> = {
  actionData?: Resolver<Maybe<Array<Maybe<ResolversTypes['ActionData']>>>, ParentType, ContextType>;
  actionState?: Resolver<ResolversTypes['ActionStates'], ParentType, ContextType>;
  blockInfo?: Resolver<Maybe<ResolversTypes['BlockInfo']>, ParentType, ContextType>;
  transactionInfo?: Resolver<Maybe<ResolversTypes['TransactionInfo']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ActionStatesResolvers<ContextType = any, ParentType extends ResolversParentTypes['ActionStates'] = ResolversParentTypes['ActionStates']> = {
  actionStateFive?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  actionStateFour?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  actionStateOne?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  actionStateThree?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  actionStateTwo?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AddAccountPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['AddAccountPayload'] = ResolversParentTypes['AddAccountPayload']> = {
  account?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AddrsAndPortsResolvers<ContextType = any, ParentType extends ResolversParentTypes['AddrsAndPorts'] = ResolversParentTypes['AddrsAndPorts']> = {
  bindIp?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  clientPort?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  externalIp?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  libp2pPort?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  peer?: Resolver<Maybe<ResolversTypes['Peer']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface AmountScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Amount'], any> {
  name: 'Amount';
}

export type AnnotatedBalanceResolvers<ContextType = any, ParentType extends ResolversParentTypes['AnnotatedBalance'] = ResolversParentTypes['AnnotatedBalance']> = {
  blockHeight?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  liquid?: Resolver<Maybe<ResolversTypes['Balance']>, ParentType, ContextType>;
  locked?: Resolver<Maybe<ResolversTypes['Balance']>, ParentType, ContextType>;
  stateHash?: Resolver<Maybe<ResolversTypes['StateHash']>, ParentType, ContextType>;
  total?: Resolver<ResolversTypes['Balance'], ParentType, ContextType>;
  unknown?: Resolver<ResolversTypes['Balance'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type AppliedResolvers<ContextType = any, ParentType extends ResolversParentTypes['Applied'] = ResolversParentTypes['Applied']> = {
  applied?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface AuthRequiredScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['AuthRequired'], any> {
  name: 'AuthRequired';
}

export type AuthorizationKindStructuredResolvers<ContextType = any, ParentType extends ResolversParentTypes['AuthorizationKindStructured'] = ResolversParentTypes['AuthorizationKindStructured']> = {
  isProved?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  isSigned?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  verificationKeyHash?: Resolver<ResolversTypes['Field'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface BalanceScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Balance'], any> {
  name: 'Balance';
}

export type BalanceChangeResolvers<ContextType = any, ParentType extends ResolversParentTypes['BalanceChange'] = ResolversParentTypes['BalanceChange']> = {
  magnitude?: Resolver<ResolversTypes['CurrencyAmount'], ParentType, ContextType>;
  sgn?: Resolver<ResolversTypes['Sign'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type BalanceIntervalResolvers<ContextType = any, ParentType extends ResolversParentTypes['BalanceInterval'] = ResolversParentTypes['BalanceInterval']> = {
  lower?: Resolver<ResolversTypes['Balance'], ParentType, ContextType>;
  upper?: Resolver<ResolversTypes['Balance'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type BlockResolvers<ContextType = any, ParentType extends ResolversParentTypes['Block'] = ResolversParentTypes['Block']> = {
  commandTransactionCount?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  creator?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  creatorAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  protocolState?: Resolver<ResolversTypes['ProtocolState'], ParentType, ContextType>;
  protocolStateProof?: Resolver<ResolversTypes['protocolStateProof'], ParentType, ContextType>;
  snarkJobs?: Resolver<Array<ResolversTypes['CompletedWork']>, ParentType, ContextType>;
  stateHash?: Resolver<ResolversTypes['StateHash'], ParentType, ContextType>;
  stateHashField?: Resolver<ResolversTypes['StateHashAsDecimal'], ParentType, ContextType>;
  transactions?: Resolver<ResolversTypes['Transactions'], ParentType, ContextType>;
  winnerAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type BlockInfoResolvers<ContextType = any, ParentType extends ResolversParentTypes['BlockInfo'] = ResolversParentTypes['BlockInfo']> = {
  chainStatus?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  distanceFromMaxBlockHeight?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  globalSlotSinceGenesis?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  globalSlotSinceHardfork?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  height?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  ledgerHash?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  parentHash?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  stateHash?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  timestamp?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type BlockProducerTimingsResolvers<ContextType = any, ParentType extends ResolversParentTypes['BlockProducerTimings'] = ResolversParentTypes['BlockProducerTimings']> = {
  generatedFromConsensusAt?: Resolver<ResolversTypes['ConsensusTimeGlobalSlot'], ParentType, ContextType>;
  globalSlotSinceGenesis?: Resolver<Array<ResolversTypes['Globalslot']>, ParentType, ContextType>;
  times?: Resolver<Array<ResolversTypes['ConsensusTime']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface BlockTimeScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['BlockTime'], any> {
  name: 'BlockTime';
}

export type BlockchainStateResolvers<ContextType = any, ParentType extends ResolversParentTypes['BlockchainState'] = ResolversParentTypes['BlockchainState']> = {
  bodyReference?: Resolver<ResolversTypes['BodyReference'], ParentType, ContextType>;
  date?: Resolver<ResolversTypes['BlockTime'], ParentType, ContextType>;
  snarkedLedgerHash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  stagedLedgerAuxHash?: Resolver<ResolversTypes['StagedLedgerAuxHash'], ParentType, ContextType>;
  stagedLedgerHash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  stagedLedgerPendingCoinbaseAux?: Resolver<ResolversTypes['PendingCoinbaseAuxHash'], ParentType, ContextType>;
  stagedLedgerPendingCoinbaseHash?: Resolver<ResolversTypes['PendingCoinbaseHash'], ParentType, ContextType>;
  stagedLedgerProofEmitted?: Resolver<Maybe<ResolversTypes['Boolean']>, ParentType, ContextType>;
  utcDate?: Resolver<ResolversTypes['BlockTime'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface BodyReferenceScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['BodyReference'], any> {
  name: 'BodyReference';
}

export interface ChainHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['ChainHash'], any> {
  name: 'ChainHash';
}

export type CompletedWorkResolvers<ContextType = any, ParentType extends ResolversParentTypes['CompletedWork'] = ResolversParentTypes['CompletedWork']> = {
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  prover?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  workIds?: Resolver<Array<ResolversTypes['Int']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ConsensusConfigurationResolvers<ContextType = any, ParentType extends ResolversParentTypes['ConsensusConfiguration'] = ResolversParentTypes['ConsensusConfiguration']> = {
  acceptableNetworkDelay?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  delta?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  epochDuration?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  genesisStateTimestamp?: Resolver<ResolversTypes['Time'], ParentType, ContextType>;
  k?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  slotDuration?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  slotsPerEpoch?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ConsensusStateResolvers<ContextType = any, ParentType extends ResolversParentTypes['ConsensusState'] = ResolversParentTypes['ConsensusState']> = {
  blockCreator?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  blockHeight?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  blockStakeWinner?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  blockchainLength?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  coinbaseReceiever?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  epoch?: Resolver<ResolversTypes['Epoch'], ParentType, ContextType>;
  epochCount?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  hasAncestorInSameCheckpointWindow?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  lastVrfOutput?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  minWindowDensity?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  nextEpochData?: Resolver<ResolversTypes['NextEpochData'], ParentType, ContextType>;
  slot?: Resolver<ResolversTypes['Slot'], ParentType, ContextType>;
  slotSinceGenesis?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
  stakingEpochData?: Resolver<ResolversTypes['StakingEpochData'], ParentType, ContextType>;
  superchargedCoinbase?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  totalCurrency?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ConsensusTimeResolvers<ContextType = any, ParentType extends ResolversParentTypes['ConsensusTime'] = ResolversParentTypes['ConsensusTime']> = {
  endTime?: Resolver<ResolversTypes['BlockTime'], ParentType, ContextType>;
  epoch?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  globalSlot?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
  slot?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  startTime?: Resolver<ResolversTypes['BlockTime'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ConsensusTimeGlobalSlotResolvers<ContextType = any, ParentType extends ResolversParentTypes['ConsensusTimeGlobalSlot'] = ResolversParentTypes['ConsensusTimeGlobalSlot']> = {
  consensusTime?: Resolver<ResolversTypes['ConsensusTime'], ParentType, ContextType>;
  globalSlotSinceGenesis?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ControlResolvers<ContextType = any, ParentType extends ResolversParentTypes['Control'] = ResolversParentTypes['Control']> = {
  proof?: Resolver<Maybe<ResolversTypes['ZkappProof']>, ParentType, ContextType>;
  signature?: Resolver<Maybe<ResolversTypes['Signature']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface CurrencyAmountScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['CurrencyAmount'], any> {
  name: 'CurrencyAmount';
}

export type CurrencyAmountIntervalResolvers<ContextType = any, ParentType extends ResolversParentTypes['CurrencyAmountInterval'] = ResolversParentTypes['CurrencyAmountInterval']> = {
  lower?: Resolver<ResolversTypes['CurrencyAmount'], ParentType, ContextType>;
  upper?: Resolver<ResolversTypes['CurrencyAmount'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type DaemonStatusResolvers<ContextType = any, ParentType extends ResolversParentTypes['DaemonStatus'] = ResolversParentTypes['DaemonStatus']> = {
  addrsAndPorts?: Resolver<ResolversTypes['AddrsAndPorts'], ParentType, ContextType>;
  blockProductionKeys?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>;
  blockchainLength?: Resolver<Maybe<ResolversTypes['Int']>, ParentType, ContextType>;
  catchupStatus?: Resolver<Maybe<Array<ResolversTypes['String']>>, ParentType, ContextType>;
  chainId?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  coinbaseReceiver?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  commitId?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  confDir?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  consensusConfiguration?: Resolver<ResolversTypes['ConsensusConfiguration'], ParentType, ContextType>;
  consensusMechanism?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  consensusTimeBestTip?: Resolver<Maybe<ResolversTypes['ConsensusTime']>, ParentType, ContextType>;
  consensusTimeNow?: Resolver<ResolversTypes['ConsensusTime'], ParentType, ContextType>;
  globalSlotSinceGenesisBestTip?: Resolver<Maybe<ResolversTypes['Int']>, ParentType, ContextType>;
  highestBlockLengthReceived?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  highestUnvalidatedBlockLengthReceived?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  histograms?: Resolver<Maybe<ResolversTypes['Histograms']>, ParentType, ContextType>;
  ledgerMerkleRoot?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  metrics?: Resolver<ResolversTypes['Metrics'], ParentType, ContextType>;
  nextBlockProduction?: Resolver<Maybe<ResolversTypes['BlockProducerTimings']>, ParentType, ContextType>;
  numAccounts?: Resolver<Maybe<ResolversTypes['Int']>, ParentType, ContextType>;
  peers?: Resolver<Array<ResolversTypes['Peer']>, ParentType, ContextType>;
  snarkWorkFee?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  snarkWorker?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  stateHash?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  syncStatus?: Resolver<ResolversTypes['SyncStatus'], ParentType, ContextType>;
  uptimeSecs?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  userCommandsSent?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type DeleteAccountPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['DeleteAccountPayload'] = ResolversParentTypes['DeleteAccountPayload']> = {
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface EpochScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Epoch'], any> {
  name: 'Epoch';
}

export type EpochDataPreconditionResolvers<ContextType = any, ParentType extends ResolversParentTypes['EpochDataPrecondition'] = ResolversParentTypes['EpochDataPrecondition']> = {
  epochLength?: Resolver<Maybe<ResolversTypes['LengthInterval']>, ParentType, ContextType>;
  ledger?: Resolver<ResolversTypes['EpochLedgerPrecondition'], ParentType, ContextType>;
  lockCheckpoint?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  seed?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  startCheckpoint?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type EpochLedgerPreconditionResolvers<ContextType = any, ParentType extends ResolversParentTypes['EpochLedgerPrecondition'] = ResolversParentTypes['EpochLedgerPrecondition']> = {
  hash?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  totalCurrency?: Resolver<Maybe<ResolversTypes['CurrencyAmountInterval']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface EpochSeedScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['EpochSeed'], any> {
  name: 'EpochSeed';
}

export type EventDataResolvers<ContextType = any, ParentType extends ResolversParentTypes['EventData'] = ResolversParentTypes['EventData']> = {
  data?: Resolver<Array<Maybe<ResolversTypes['String']>>, ParentType, ContextType>;
  transactionInfo?: Resolver<Maybe<ResolversTypes['TransactionInfo']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type EventOutputResolvers<ContextType = any, ParentType extends ResolversParentTypes['EventOutput'] = ResolversParentTypes['EventOutput']> = {
  blockInfo?: Resolver<Maybe<ResolversTypes['BlockInfo']>, ParentType, ContextType>;
  eventData?: Resolver<Maybe<Array<Maybe<ResolversTypes['EventData']>>>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ExportLogsPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['ExportLogsPayload'] = ResolversParentTypes['ExportLogsPayload']> = {
  exportLogs?: Resolver<ResolversTypes['TarFile'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface ExtensionalBlockScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['ExtensionalBlock'], any> {
  name: 'ExtensionalBlock';
}

export interface FeeScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Fee'], any> {
  name: 'Fee';
}

export type FeePayerBodyResolvers<ContextType = any, ParentType extends ResolversParentTypes['FeePayerBody'] = ResolversParentTypes['FeePayerBody']> = {
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  nonce?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  validUntil?: Resolver<Maybe<ResolversTypes['UInt32']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type FeeTransferResolvers<ContextType = any, ParentType extends ResolversParentTypes['FeeTransfer'] = ResolversParentTypes['FeeTransfer']> = {
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  recipient?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  type?: Resolver<ResolversTypes['FeeTransferType'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface FeeTransferTypeScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['FeeTransferType'], any> {
  name: 'FeeTransferType';
}

export interface FieldScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Field'], any> {
  name: 'Field';
}

export interface FieldElemScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['FieldElem'], any> {
  name: 'FieldElem';
}

export type GenesisConstantsResolvers<ContextType = any, ParentType extends ResolversParentTypes['GenesisConstants'] = ResolversParentTypes['GenesisConstants']> = {
  accountCreationFee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  coinbase?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface GlobalSlotScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['GlobalSlot'], any> {
  name: 'GlobalSlot';
}

export type GlobalSlotIntervalResolvers<ContextType = any, ParentType extends ResolversParentTypes['GlobalSlotInterval'] = ResolversParentTypes['GlobalSlotInterval']> = {
  lower?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  upper?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface GlobalslotScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Globalslot'], any> {
  name: 'Globalslot';
}

export type HistogramResolvers<ContextType = any, ParentType extends ResolversParentTypes['Histogram'] = ResolversParentTypes['Histogram']> = {
  intervals?: Resolver<Array<ResolversTypes['Interval']>, ParentType, ContextType>;
  overflow?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  underflow?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  values?: Resolver<Array<ResolversTypes['Int']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type HistogramsResolvers<ContextType = any, ParentType extends ResolversParentTypes['Histograms'] = ResolversParentTypes['Histograms']> = {
  acceptedTransitionLocalLatency?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  acceptedTransitionRemoteLatency?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  externalTransitionLatency?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  rpcTimings?: Resolver<ResolversTypes['RpcTimings'], ParentType, ContextType>;
  snarkWorkerMergeTime?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  snarkWorkerTransitionTime?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ImportAccountPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['ImportAccountPayload'] = ResolversParentTypes['ImportAccountPayload']> = {
  alreadyImported?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  success?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface IndexScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Index'], any> {
  name: 'Index';
}

export interface InetAddrScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['InetAddr'], any> {
  name: 'InetAddr';
}

export type IntervalResolvers<ContextType = any, ParentType extends ResolversParentTypes['Interval'] = ResolversParentTypes['Interval']> = {
  start?: Resolver<ResolversTypes['Span'], ParentType, ContextType>;
  stop?: Resolver<ResolversTypes['Span'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface JsonScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['JSON'], any> {
  name: 'JSON';
}

export interface LedgerHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['LedgerHash'], any> {
  name: 'LedgerHash';
}

export interface LengthScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Length'], any> {
  name: 'Length';
}

export type LengthIntervalResolvers<ContextType = any, ParentType extends ResolversParentTypes['LengthInterval'] = ResolversParentTypes['LengthInterval']> = {
  lower?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  upper?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type LockPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['LockPayload'] = ResolversParentTypes['LockPayload']> = {
  account?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type MayUseTokenResolvers<ContextType = any, ParentType extends ResolversParentTypes['MayUseToken'] = ResolversParentTypes['MayUseToken']> = {
  inheritFromParent?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  parentsOwnToken?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface MemoScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Memo'], any> {
  name: 'Memo';
}

export type MerklePathElementResolvers<ContextType = any, ParentType extends ResolversParentTypes['MerklePathElement'] = ResolversParentTypes['MerklePathElement']> = {
  left?: Resolver<Maybe<ResolversTypes['FieldElem']>, ParentType, ContextType>;
  right?: Resolver<Maybe<ResolversTypes['FieldElem']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type MetricsResolvers<ContextType = any, ParentType extends ResolversParentTypes['Metrics'] = ResolversParentTypes['Metrics']> = {
  blockProductionDelay?: Resolver<Array<ResolversTypes['Int']>, ParentType, ContextType>;
  transactionPoolDiffBroadcasted?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  transactionPoolDiffReceived?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  transactionPoolSize?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  transactionsAddedToPool?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type NetworkPeerPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['NetworkPeerPayload'] = ResolversParentTypes['NetworkPeerPayload']> = {
  host?: Resolver<ResolversTypes['InetAddr'], ParentType, ContextType>;
  libp2pPort?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  peerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type NetworkPreconditionResolvers<ContextType = any, ParentType extends ResolversParentTypes['NetworkPrecondition'] = ResolversParentTypes['NetworkPrecondition']> = {
  blockchainLength?: Resolver<Maybe<ResolversTypes['LengthInterval']>, ParentType, ContextType>;
  globalSlotSinceGenesis?: Resolver<Maybe<ResolversTypes['GlobalSlotInterval']>, ParentType, ContextType>;
  minWindowDensity?: Resolver<Maybe<ResolversTypes['LengthInterval']>, ParentType, ContextType>;
  nextEpochData?: Resolver<ResolversTypes['EpochDataPrecondition'], ParentType, ContextType>;
  snarkedLedgerHash?: Resolver<Maybe<ResolversTypes['Field']>, ParentType, ContextType>;
  stakingEpochData?: Resolver<ResolversTypes['EpochDataPrecondition'], ParentType, ContextType>;
  totalCurrency?: Resolver<Maybe<ResolversTypes['CurrencyAmountInterval']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type NextEpochDataResolvers<ContextType = any, ParentType extends ResolversParentTypes['NextEpochData'] = ResolversParentTypes['NextEpochData']> = {
  epochLength?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  ledger?: Resolver<ResolversTypes['epochLedger'], ParentType, ContextType>;
  lockCheckpoint?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  seed?: Resolver<ResolversTypes['EpochSeed'], ParentType, ContextType>;
  startCheckpoint?: Resolver<ResolversTypes['StateHash'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type NonceIntervalResolvers<ContextType = any, ParentType extends ResolversParentTypes['NonceInterval'] = ResolversParentTypes['NonceInterval']> = {
  lower?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  upper?: Resolver<ResolversTypes['UInt32'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type PeerResolvers<ContextType = any, ParentType extends ResolversParentTypes['Peer'] = ResolversParentTypes['Peer']> = {
  host?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  libp2pPort?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  peerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface PendingCoinbaseAuxHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['PendingCoinbaseAuxHash'], any> {
  name: 'PendingCoinbaseAuxHash';
}

export interface PendingCoinbaseHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['PendingCoinbaseHash'], any> {
  name: 'PendingCoinbaseHash';
}

export type PendingSnarkWorkResolvers<ContextType = any, ParentType extends ResolversParentTypes['PendingSnarkWork'] = ResolversParentTypes['PendingSnarkWork']> = {
  workBundle?: Resolver<Array<ResolversTypes['WorkDescription']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type PermissionsResolvers<ContextType = any, ParentType extends ResolversParentTypes['Permissions'] = ResolversParentTypes['Permissions']> = {
  access?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  editActionState?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  editState?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  incrementNonce?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  receive?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  send?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setDelegate?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setPermissions?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setTiming?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setTokenSymbol?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setVerificationKey?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setVotingFor?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  setZkappUri?: Resolver<ResolversTypes['AuthRequired'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface PrecomputedBlockScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['PrecomputedBlock'], any> {
  name: 'PrecomputedBlock';
}

export interface PrecomputedBlockProofScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['PrecomputedBlockProof'], any> {
  name: 'PrecomputedBlockProof';
}

export type PreconditionsResolvers<ContextType = any, ParentType extends ResolversParentTypes['Preconditions'] = ResolversParentTypes['Preconditions']> = {
  account?: Resolver<ResolversTypes['AccountPrecondition'], ParentType, ContextType>;
  network?: Resolver<ResolversTypes['NetworkPrecondition'], ParentType, ContextType>;
  validWhile?: Resolver<Maybe<ResolversTypes['GlobalSlotInterval']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface PrivateKeyScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['PrivateKey'], any> {
  name: 'PrivateKey';
}

export type ProtocolStateResolvers<ContextType = any, ParentType extends ResolversParentTypes['ProtocolState'] = ResolversParentTypes['ProtocolState']> = {
  blockchainState?: Resolver<ResolversTypes['BlockchainState'], ParentType, ContextType>;
  consensusState?: Resolver<ResolversTypes['ConsensusState'], ParentType, ContextType>;
  previousStateHash?: Resolver<ResolversTypes['StateHash'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface PublicKeyScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['PublicKey'], any> {
  name: 'PublicKey';
}

export type ReloadAccountsPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['ReloadAccountsPayload'] = ResolversParentTypes['ReloadAccountsPayload']> = {
  success?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface RosettaTransactionScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['RosettaTransaction'], any> {
  name: 'RosettaTransaction';
}

export type RpcPairResolvers<ContextType = any, ParentType extends ResolversParentTypes['RpcPair'] = ResolversParentTypes['RpcPair']> = {
  dispatch?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  impl?: Resolver<Maybe<ResolversTypes['Histogram']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type RpcTimingsResolvers<ContextType = any, ParentType extends ResolversParentTypes['RpcTimings'] = ResolversParentTypes['RpcTimings']> = {
  answerSyncLedgerQuery?: Resolver<ResolversTypes['RpcPair'], ParentType, ContextType>;
  getAncestry?: Resolver<ResolversTypes['RpcPair'], ParentType, ContextType>;
  getStagedLedgerAux?: Resolver<ResolversTypes['RpcPair'], ParentType, ContextType>;
  getTransitionChain?: Resolver<ResolversTypes['RpcPair'], ParentType, ContextType>;
  getTransitionChainProof?: Resolver<ResolversTypes['RpcPair'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SendDelegationPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SendDelegationPayload'] = ResolversParentTypes['SendDelegationPayload']> = {
  delegation?: Resolver<ResolversTypes['UserCommand'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SendPaymentPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SendPaymentPayload'] = ResolversParentTypes['SendPaymentPayload']> = {
  payment?: Resolver<ResolversTypes['UserCommand'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SendRosettaTransactionPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SendRosettaTransactionPayload'] = ResolversParentTypes['SendRosettaTransactionPayload']> = {
  userCommand?: Resolver<ResolversTypes['UserCommand'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface SendTestZkappInputScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['SendTestZkappInput'], any> {
  name: 'SendTestZkappInput';
}

export type SendZkappPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SendZkappPayload'] = ResolversParentTypes['SendZkappPayload']> = {
  zkapp?: Resolver<ResolversTypes['ZkappCommandResult'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SetCoinbaseReceiverPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SetCoinbaseReceiverPayload'] = ResolversParentTypes['SetCoinbaseReceiverPayload']> = {
  currentCoinbaseReceiver?: Resolver<Maybe<ResolversTypes['PublicKey']>, ParentType, ContextType>;
  lastCoinbaseReceiver?: Resolver<Maybe<ResolversTypes['PublicKey']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SetConnectionGatingConfigPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SetConnectionGatingConfigPayload'] = ResolversParentTypes['SetConnectionGatingConfigPayload']> = {
  bannedPeers?: Resolver<Array<ResolversTypes['NetworkPeerPayload']>, ParentType, ContextType>;
  isolate?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  trustedPeers?: Resolver<Array<ResolversTypes['NetworkPeerPayload']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SetSnarkWorkFeePayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SetSnarkWorkFeePayload'] = ResolversParentTypes['SetSnarkWorkFeePayload']> = {
  lastFee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type SetSnarkWorkerPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['SetSnarkWorkerPayload'] = ResolversParentTypes['SetSnarkWorkerPayload']> = {
  lastSnarkWorker?: Resolver<Maybe<ResolversTypes['PublicKey']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface SignScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Sign'], any> {
  name: 'Sign';
}

export interface SignatureScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Signature'], any> {
  name: 'Signature';
}

export type SignedFeeResolvers<ContextType = any, ParentType extends ResolversParentTypes['SignedFee'] = ResolversParentTypes['SignedFee']> = {
  feeMagnitude?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  sign?: Resolver<ResolversTypes['sign'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface SlotScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Slot'], any> {
  name: 'Slot';
}

export type SnarkWorkerResolvers<ContextType = any, ParentType extends ResolversParentTypes['SnarkWorker'] = ResolversParentTypes['SnarkWorker']> = {
  account?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  key?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface SpanScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Span'], any> {
  name: 'Span';
}

export interface StagedLedgerAuxHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['StagedLedgerAuxHash'], any> {
  name: 'StagedLedgerAuxHash';
}

export type StakingEpochDataResolvers<ContextType = any, ParentType extends ResolversParentTypes['StakingEpochData'] = ResolversParentTypes['StakingEpochData']> = {
  epochLength?: Resolver<ResolversTypes['Length'], ParentType, ContextType>;
  ledger?: Resolver<ResolversTypes['epochLedger'], ParentType, ContextType>;
  lockCheckpoint?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  seed?: Resolver<ResolversTypes['EpochSeed'], ParentType, ContextType>;
  startCheckpoint?: Resolver<ResolversTypes['StateHash'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface StateHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['StateHash'], any> {
  name: 'StateHash';
}

export interface StateHashAsDecimalScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['StateHashAsDecimal'], any> {
  name: 'StateHashAsDecimal';
}

export type TarFileResolvers<ContextType = any, ParentType extends ResolversParentTypes['TarFile'] = ResolversParentTypes['TarFile']> = {
  tarfile?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface TimeScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['Time'], any> {
  name: 'Time';
}

export type TimingResolvers<ContextType = any, ParentType extends ResolversParentTypes['Timing'] = ResolversParentTypes['Timing']> = {
  cliffAmount?: Resolver<ResolversTypes['CurrencyAmount'], ParentType, ContextType>;
  cliffTime?: Resolver<ResolversTypes['GlobalSlot'], ParentType, ContextType>;
  initialMinimumBalance?: Resolver<ResolversTypes['Balance'], ParentType, ContextType>;
  vestingIncrement?: Resolver<ResolversTypes['CurrencyAmount'], ParentType, ContextType>;
  vestingPeriod?: Resolver<ResolversTypes['GlobalSlot'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface TokenIdScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['TokenId'], any> {
  name: 'TokenId';
}

export interface TransactionHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['TransactionHash'], any> {
  name: 'TransactionHash';
}

export interface TransactionIdScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['TransactionId'], any> {
  name: 'TransactionId';
}

export type TransactionInfoResolvers<ContextType = any, ParentType extends ResolversParentTypes['TransactionInfo'] = ResolversParentTypes['TransactionInfo']> = {
  authorizationKind?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  hash?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  memo?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  status?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface TransactionStatusFailureScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['TransactionStatusFailure'], any> {
  name: 'TransactionStatusFailure';
}

export type TransactionsResolvers<ContextType = any, ParentType extends ResolversParentTypes['Transactions'] = ResolversParentTypes['Transactions']> = {
  coinbase?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  coinbaseReceiverAccount?: Resolver<Maybe<ResolversTypes['Account']>, ParentType, ContextType>;
  feeTransfer?: Resolver<Array<ResolversTypes['FeeTransfer']>, ParentType, ContextType>;
  userCommands?: Resolver<Array<ResolversTypes['UserCommand']>, ParentType, ContextType>;
  zkappCommands?: Resolver<Array<ResolversTypes['ZkappCommandResult']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type TrustStatusPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['TrustStatusPayload'] = ResolversParentTypes['TrustStatusPayload']> = {
  bannedStatus?: Resolver<Maybe<ResolversTypes['Time']>, ParentType, ContextType>;
  ipAddr?: Resolver<ResolversTypes['InetAddr'], ParentType, ContextType>;
  peerId?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  trust?: Resolver<ResolversTypes['Float'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface UInt32ScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['UInt32'], any> {
  name: 'UInt32';
}

export interface UInt64ScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['UInt64'], any> {
  name: 'UInt64';
}

export type UnlockPayloadResolvers<ContextType = any, ParentType extends ResolversParentTypes['UnlockPayload'] = ResolversParentTypes['UnlockPayload']> = {
  account?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type UserCommandResolvers<ContextType = any, ParentType extends ResolversParentTypes['UserCommand'] = ResolversParentTypes['UserCommand']> = {
  __resolveType: TypeResolveFn<'UserCommandDelegation' | 'UserCommandPayment', ParentType, ContextType>;
  amount?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  failureReason?: Resolver<Maybe<ResolversTypes['TransactionStatusFailure']>, ParentType, ContextType>;
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  feePayer?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  feeToken?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  from?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  fromAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  hash?: Resolver<ResolversTypes['TransactionHash'], ParentType, ContextType>;
  id?: Resolver<ResolversTypes['TransactionId'], ParentType, ContextType>;
  isDelegation?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  kind?: Resolver<ResolversTypes['UserCommandKind'], ParentType, ContextType>;
  memo?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  nonce?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  receiver?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  source?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  to?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  toAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  token?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  validUntil?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
};

export type UserCommandDelegationResolvers<ContextType = any, ParentType extends ResolversParentTypes['UserCommandDelegation'] = ResolversParentTypes['UserCommandDelegation']> = {
  amount?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  delegatee?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  delegator?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  failureReason?: Resolver<Maybe<ResolversTypes['TransactionStatusFailure']>, ParentType, ContextType>;
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  feePayer?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  feeToken?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  from?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  fromAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  hash?: Resolver<ResolversTypes['TransactionHash'], ParentType, ContextType>;
  id?: Resolver<ResolversTypes['TransactionId'], ParentType, ContextType>;
  isDelegation?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  kind?: Resolver<ResolversTypes['UserCommandKind'], ParentType, ContextType>;
  memo?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  nonce?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  receiver?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  source?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  to?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  toAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  token?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  validUntil?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface UserCommandKindScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['UserCommandKind'], any> {
  name: 'UserCommandKind';
}

export type UserCommandPaymentResolvers<ContextType = any, ParentType extends ResolversParentTypes['UserCommandPayment'] = ResolversParentTypes['UserCommandPayment']> = {
  amount?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  failureReason?: Resolver<Maybe<ResolversTypes['TransactionStatusFailure']>, ParentType, ContextType>;
  fee?: Resolver<ResolversTypes['Fee'], ParentType, ContextType>;
  feePayer?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  feeToken?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  from?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  fromAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  hash?: Resolver<ResolversTypes['TransactionHash'], ParentType, ContextType>;
  id?: Resolver<ResolversTypes['TransactionId'], ParentType, ContextType>;
  isDelegation?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType>;
  kind?: Resolver<ResolversTypes['UserCommandKind'], ParentType, ContextType>;
  memo?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  nonce?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  receiver?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  source?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  to?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  toAccount?: Resolver<ResolversTypes['Account'], ParentType, ContextType>;
  token?: Resolver<ResolversTypes['TokenId'], ParentType, ContextType>;
  validUntil?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface VerificationKeyScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['VerificationKey'], any> {
  name: 'VerificationKey';
}

export interface VerificationKeyHashScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['VerificationKeyHash'], any> {
  name: 'VerificationKeyHash';
}

export type VerificationKeyWithHashResolvers<ContextType = any, ParentType extends ResolversParentTypes['VerificationKeyWithHash'] = ResolversParentTypes['VerificationKeyWithHash']> = {
  data?: Resolver<ResolversTypes['VerificationKey'], ParentType, ContextType>;
  hash?: Resolver<ResolversTypes['Field'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type VrfEvaluationResolvers<ContextType = any, ParentType extends ResolversParentTypes['VrfEvaluation'] = ResolversParentTypes['VrfEvaluation']> = {
  c?: Resolver<ResolversTypes['VrfScalar'], ParentType, ContextType>;
  message?: Resolver<ResolversTypes['VrfMessage'], ParentType, ContextType>;
  publicKey?: Resolver<ResolversTypes['PublicKey'], ParentType, ContextType>;
  s?: Resolver<ResolversTypes['VrfScalar'], ParentType, ContextType>;
  scaledMessageHash?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>;
  thresholdMet?: Resolver<Maybe<ResolversTypes['Boolean']>, ParentType, ContextType, Partial<VrfEvaluationThresholdMetArgs>>;
  vrfOutput?: Resolver<Maybe<ResolversTypes['VrfOutputTruncated']>, ParentType, ContextType>;
  vrfOutputFractional?: Resolver<Maybe<ResolversTypes['Float']>, ParentType, ContextType>;
  vrfThreshold?: Resolver<Maybe<ResolversTypes['VrfThreshold']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type VrfMessageResolvers<ContextType = any, ParentType extends ResolversParentTypes['VrfMessage'] = ResolversParentTypes['VrfMessage']> = {
  delegatorIndex?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  epochSeed?: Resolver<ResolversTypes['EpochSeed'], ParentType, ContextType>;
  globalSlot?: Resolver<ResolversTypes['Globalslot'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface VrfOutputTruncatedScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['VrfOutputTruncated'], any> {
  name: 'VrfOutputTruncated';
}

export interface VrfScalarScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['VrfScalar'], any> {
  name: 'VrfScalar';
}

export type VrfThresholdResolvers<ContextType = any, ParentType extends ResolversParentTypes['VrfThreshold'] = ResolversParentTypes['VrfThreshold']> = {
  delegatedStake?: Resolver<ResolversTypes['Balance'], ParentType, ContextType>;
  totalStake?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type WorkDescriptionResolvers<ContextType = any, ParentType extends ResolversParentTypes['WorkDescription'] = ResolversParentTypes['WorkDescription']> = {
  feeExcess?: Resolver<ResolversTypes['SignedFee'], ParentType, ContextType>;
  sourceFirstPassLedgerHash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  sourceSecondPassLedgerHash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  supplyChange?: Resolver<ResolversTypes['SignedFee'], ParentType, ContextType>;
  supplyIncrease?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  targetFirstPassLedgerHash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  targetSecondPassLedgerHash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  workId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ZkappAccountUpdateResolvers<ContextType = any, ParentType extends ResolversParentTypes['ZkappAccountUpdate'] = ResolversParentTypes['ZkappAccountUpdate']> = {
  authorization?: Resolver<ResolversTypes['Control'], ParentType, ContextType>;
  body?: Resolver<ResolversTypes['AccountUpdateBody'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ZkappCommandResolvers<ContextType = any, ParentType extends ResolversParentTypes['ZkappCommand'] = ResolversParentTypes['ZkappCommand']> = {
  accountUpdates?: Resolver<Array<ResolversTypes['ZkappAccountUpdate']>, ParentType, ContextType>;
  feePayer?: Resolver<ResolversTypes['ZkappFeePayer'], ParentType, ContextType>;
  memo?: Resolver<ResolversTypes['Memo'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ZkappCommandFailureReasonResolvers<ContextType = any, ParentType extends ResolversParentTypes['ZkappCommandFailureReason'] = ResolversParentTypes['ZkappCommandFailureReason']> = {
  failures?: Resolver<Array<ResolversTypes['TransactionStatusFailure']>, ParentType, ContextType>;
  index?: Resolver<Maybe<ResolversTypes['Index']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ZkappCommandResultResolvers<ContextType = any, ParentType extends ResolversParentTypes['ZkappCommandResult'] = ResolversParentTypes['ZkappCommandResult']> = {
  failureReason?: Resolver<Maybe<Array<Maybe<ResolversTypes['ZkappCommandFailureReason']>>>, ParentType, ContextType>;
  hash?: Resolver<ResolversTypes['TransactionHash'], ParentType, ContextType>;
  id?: Resolver<ResolversTypes['TransactionId'], ParentType, ContextType>;
  zkappCommand?: Resolver<ResolversTypes['ZkappCommand'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ZkappFeePayerResolvers<ContextType = any, ParentType extends ResolversParentTypes['ZkappFeePayer'] = ResolversParentTypes['ZkappFeePayer']> = {
  authorization?: Resolver<ResolversTypes['Signature'], ParentType, ContextType>;
  body?: Resolver<ResolversTypes['FeePayerBody'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export interface ZkappProofScalarConfig extends GraphQLScalarTypeConfig<ResolversTypes['ZkappProof'], any> {
  name: 'ZkappProof';
}

export type EpochLedgerResolvers<ContextType = any, ParentType extends ResolversParentTypes['epochLedger'] = ResolversParentTypes['epochLedger']> = {
  hash?: Resolver<ResolversTypes['LedgerHash'], ParentType, ContextType>;
  totalCurrency?: Resolver<ResolversTypes['Amount'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type MutationResolvers<ContextType = any, ParentType extends ResolversParentTypes['mutation'] = ResolversParentTypes['mutation']> = {
  addWallet?: Resolver<ResolversTypes['AddAccountPayload'], ParentType, ContextType, RequireFields<MutationAddWalletArgs, 'input'>>;
  deleteWallet?: Resolver<ResolversTypes['DeleteAccountPayload'], ParentType, ContextType, RequireFields<MutationDeleteWalletArgs, 'input'>>;
  internalSendZkapp?: Resolver<ResolversTypes['SendZkappPayload'], ParentType, ContextType, RequireFields<MutationInternalSendZkappArgs, 'zkappCommand'>>;
  lockWallet?: Resolver<ResolversTypes['LockPayload'], ParentType, ContextType, RequireFields<MutationLockWalletArgs, 'input'>>;
  mockZkapp?: Resolver<ResolversTypes['SendZkappPayload'], ParentType, ContextType, RequireFields<MutationMockZkappArgs, 'input'>>;
  reloadWallets?: Resolver<ResolversTypes['ReloadAccountsPayload'], ParentType, ContextType>;
  sendDelegation?: Resolver<ResolversTypes['SendDelegationPayload'], ParentType, ContextType, RequireFields<MutationSendDelegationArgs, 'input'>>;
  sendPayment?: Resolver<ResolversTypes['SendPaymentPayload'], ParentType, ContextType, RequireFields<MutationSendPaymentArgs, 'input'>>;
  sendRosettaTransaction?: Resolver<ResolversTypes['SendRosettaTransactionPayload'], ParentType, ContextType, RequireFields<MutationSendRosettaTransactionArgs, 'input'>>;
  sendTestPayments?: Resolver<ResolversTypes['Int'], ParentType, ContextType, RequireFields<MutationSendTestPaymentsArgs, 'amount' | 'fee' | 'receiver' | 'repeat_count' | 'repeat_delay_ms' | 'senders'>>;
  sendZkapp?: Resolver<ResolversTypes['SendZkappPayload'], ParentType, ContextType, RequireFields<MutationSendZkappArgs, 'input'>>;
  unlockWallet?: Resolver<ResolversTypes['UnlockPayload'], ParentType, ContextType, RequireFields<MutationUnlockWalletArgs, 'input'>>;
};

export type ProtocolStateProofResolvers<ContextType = any, ParentType extends ResolversParentTypes['protocolStateProof'] = ResolversParentTypes['protocolStateProof']> = {
  base64?: Resolver<Maybe<ResolversTypes['PrecomputedBlockProof']>, ParentType, ContextType>;
  json?: Resolver<Maybe<ResolversTypes['JSON']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type QueryResolvers<ContextType = any, ParentType extends ResolversParentTypes['query'] = ResolversParentTypes['query']> = {
  account?: Resolver<Maybe<ResolversTypes['Account']>, ParentType, ContextType, RequireFields<QueryAccountArgs, 'publicKey'>>;
  accounts?: Resolver<Array<ResolversTypes['Account']>, ParentType, ContextType, RequireFields<QueryAccountsArgs, 'publicKey'>>;
  actions?: Resolver<Array<Maybe<ResolversTypes['ActionOutput']>>, ParentType, ContextType, RequireFields<QueryActionsArgs, 'input'>>;
  bestChain?: Resolver<Maybe<Array<ResolversTypes['Block']>>, ParentType, ContextType, Partial<QueryBestChainArgs>>;
  block?: Resolver<ResolversTypes['Block'], ParentType, ContextType, Partial<QueryBlockArgs>>;
  blockchainVerificationKey?: Resolver<ResolversTypes['JSON'], ParentType, ContextType>;
  checkVrf?: Resolver<ResolversTypes['VrfEvaluation'], ParentType, ContextType, RequireFields<QueryCheckVrfArgs, 'input'>>;
  daemonStatus?: Resolver<ResolversTypes['DaemonStatus'], ParentType, ContextType>;
  evaluateVrf?: Resolver<ResolversTypes['VrfEvaluation'], ParentType, ContextType, RequireFields<QueryEvaluateVrfArgs, 'message' | 'publicKey'>>;
  events?: Resolver<Array<Maybe<ResolversTypes['EventOutput']>>, ParentType, ContextType, RequireFields<QueryEventsArgs, 'input'>>;
  genesisBlock?: Resolver<ResolversTypes['Block'], ParentType, ContextType>;
  genesisConstants?: Resolver<ResolversTypes['GenesisConstants'], ParentType, ContextType>;
  getPeers?: Resolver<Array<ResolversTypes['Peer']>, ParentType, ContextType>;
  initialPeers?: Resolver<Array<ResolversTypes['String']>, ParentType, ContextType>;
  ownedWallets?: Resolver<Array<ResolversTypes['Account']>, ParentType, ContextType>;
  pendingSnarkWork?: Resolver<Array<ResolversTypes['PendingSnarkWork']>, ParentType, ContextType>;
  pooledUserCommands?: Resolver<Array<ResolversTypes['UserCommand']>, ParentType, ContextType, Partial<QueryPooledUserCommandsArgs>>;
  pooledZkappCommands?: Resolver<Array<ResolversTypes['ZkappCommandResult']>, ParentType, ContextType, Partial<QueryPooledZkappCommandsArgs>>;
  snarkPool?: Resolver<Array<ResolversTypes['CompletedWork']>, ParentType, ContextType>;
  syncStatus?: Resolver<ResolversTypes['SyncStatus'], ParentType, ContextType>;
  timeOffset?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  tokenAccounts?: Resolver<Array<ResolversTypes['Account']>, ParentType, ContextType, RequireFields<QueryTokenAccountsArgs, 'tokenId'>>;
  tokenOwner?: Resolver<Maybe<ResolversTypes['Account']>, ParentType, ContextType, RequireFields<QueryTokenOwnerArgs, 'tokenId'>>;
  transactionStatus?: Resolver<ResolversTypes['TransactionStatus'], ParentType, ContextType, Partial<QueryTransactionStatusArgs>>;
  trustStatus?: Resolver<Maybe<Array<ResolversTypes['TrustStatusPayload']>>, ParentType, ContextType, RequireFields<QueryTrustStatusArgs, 'ipAddress'>>;
  trustStatusAll?: Resolver<Array<ResolversTypes['TrustStatusPayload']>, ParentType, ContextType>;
  validatePayment?: Resolver<ResolversTypes['Boolean'], ParentType, ContextType, RequireFields<QueryValidatePaymentArgs, 'input'>>;
  version?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  wallet?: Resolver<Maybe<ResolversTypes['Account']>, ParentType, ContextType, RequireFields<QueryWalletArgs, 'publicKey'>>;
};

export type SubscriptionResolvers<ContextType = any, ParentType extends ResolversParentTypes['subscription'] = ResolversParentTypes['subscription']> = {
  chainReorganization?: SubscriptionResolver<ResolversTypes['ChainReorganizationStatus'], "chainReorganization", ParentType, ContextType>;
  newBlock?: SubscriptionResolver<ResolversTypes['Block'], "newBlock", ParentType, ContextType, Partial<SubscriptionNewBlockArgs>>;
  newSyncUpdate?: SubscriptionResolver<ResolversTypes['SyncStatus'], "newSyncUpdate", ParentType, ContextType>;
};

export type Resolvers<ContextType = any> = {
  Account?: AccountResolvers<ContextType>;
  AccountNonce?: GraphQLScalarType;
  AccountPermissions?: AccountPermissionsResolvers<ContextType>;
  AccountPrecondition?: AccountPreconditionResolvers<ContextType>;
  AccountTiming?: AccountTimingResolvers<ContextType>;
  AccountUpdateBody?: AccountUpdateBodyResolvers<ContextType>;
  AccountUpdateModification?: AccountUpdateModificationResolvers<ContextType>;
  AccountVerificationKeyWithHash?: AccountVerificationKeyWithHashResolvers<ContextType>;
  Action?: GraphQLScalarType;
  ActionData?: ActionDataResolvers<ContextType>;
  ActionOutput?: ActionOutputResolvers<ContextType>;
  ActionStates?: ActionStatesResolvers<ContextType>;
  AddAccountPayload?: AddAccountPayloadResolvers<ContextType>;
  AddrsAndPorts?: AddrsAndPortsResolvers<ContextType>;
  Amount?: GraphQLScalarType;
  AnnotatedBalance?: AnnotatedBalanceResolvers<ContextType>;
  Applied?: AppliedResolvers<ContextType>;
  AuthRequired?: GraphQLScalarType;
  AuthorizationKindStructured?: AuthorizationKindStructuredResolvers<ContextType>;
  Balance?: GraphQLScalarType;
  BalanceChange?: BalanceChangeResolvers<ContextType>;
  BalanceInterval?: BalanceIntervalResolvers<ContextType>;
  Block?: BlockResolvers<ContextType>;
  BlockInfo?: BlockInfoResolvers<ContextType>;
  BlockProducerTimings?: BlockProducerTimingsResolvers<ContextType>;
  BlockTime?: GraphQLScalarType;
  BlockchainState?: BlockchainStateResolvers<ContextType>;
  BodyReference?: GraphQLScalarType;
  ChainHash?: GraphQLScalarType;
  CompletedWork?: CompletedWorkResolvers<ContextType>;
  ConsensusConfiguration?: ConsensusConfigurationResolvers<ContextType>;
  ConsensusState?: ConsensusStateResolvers<ContextType>;
  ConsensusTime?: ConsensusTimeResolvers<ContextType>;
  ConsensusTimeGlobalSlot?: ConsensusTimeGlobalSlotResolvers<ContextType>;
  Control?: ControlResolvers<ContextType>;
  CurrencyAmount?: GraphQLScalarType;
  CurrencyAmountInterval?: CurrencyAmountIntervalResolvers<ContextType>;
  DaemonStatus?: DaemonStatusResolvers<ContextType>;
  DeleteAccountPayload?: DeleteAccountPayloadResolvers<ContextType>;
  Epoch?: GraphQLScalarType;
  EpochDataPrecondition?: EpochDataPreconditionResolvers<ContextType>;
  EpochLedgerPrecondition?: EpochLedgerPreconditionResolvers<ContextType>;
  EpochSeed?: GraphQLScalarType;
  EventData?: EventDataResolvers<ContextType>;
  EventOutput?: EventOutputResolvers<ContextType>;
  ExportLogsPayload?: ExportLogsPayloadResolvers<ContextType>;
  ExtensionalBlock?: GraphQLScalarType;
  Fee?: GraphQLScalarType;
  FeePayerBody?: FeePayerBodyResolvers<ContextType>;
  FeeTransfer?: FeeTransferResolvers<ContextType>;
  FeeTransferType?: GraphQLScalarType;
  Field?: GraphQLScalarType;
  FieldElem?: GraphQLScalarType;
  GenesisConstants?: GenesisConstantsResolvers<ContextType>;
  GlobalSlot?: GraphQLScalarType;
  GlobalSlotInterval?: GlobalSlotIntervalResolvers<ContextType>;
  Globalslot?: GraphQLScalarType;
  Histogram?: HistogramResolvers<ContextType>;
  Histograms?: HistogramsResolvers<ContextType>;
  ImportAccountPayload?: ImportAccountPayloadResolvers<ContextType>;
  Index?: GraphQLScalarType;
  InetAddr?: GraphQLScalarType;
  Interval?: IntervalResolvers<ContextType>;
  JSON?: GraphQLScalarType;
  LedgerHash?: GraphQLScalarType;
  Length?: GraphQLScalarType;
  LengthInterval?: LengthIntervalResolvers<ContextType>;
  LockPayload?: LockPayloadResolvers<ContextType>;
  MayUseToken?: MayUseTokenResolvers<ContextType>;
  Memo?: GraphQLScalarType;
  MerklePathElement?: MerklePathElementResolvers<ContextType>;
  Metrics?: MetricsResolvers<ContextType>;
  NetworkPeerPayload?: NetworkPeerPayloadResolvers<ContextType>;
  NetworkPrecondition?: NetworkPreconditionResolvers<ContextType>;
  NextEpochData?: NextEpochDataResolvers<ContextType>;
  NonceInterval?: NonceIntervalResolvers<ContextType>;
  Peer?: PeerResolvers<ContextType>;
  PendingCoinbaseAuxHash?: GraphQLScalarType;
  PendingCoinbaseHash?: GraphQLScalarType;
  PendingSnarkWork?: PendingSnarkWorkResolvers<ContextType>;
  Permissions?: PermissionsResolvers<ContextType>;
  PrecomputedBlock?: GraphQLScalarType;
  PrecomputedBlockProof?: GraphQLScalarType;
  Preconditions?: PreconditionsResolvers<ContextType>;
  PrivateKey?: GraphQLScalarType;
  ProtocolState?: ProtocolStateResolvers<ContextType>;
  PublicKey?: GraphQLScalarType;
  ReloadAccountsPayload?: ReloadAccountsPayloadResolvers<ContextType>;
  RosettaTransaction?: GraphQLScalarType;
  RpcPair?: RpcPairResolvers<ContextType>;
  RpcTimings?: RpcTimingsResolvers<ContextType>;
  SendDelegationPayload?: SendDelegationPayloadResolvers<ContextType>;
  SendPaymentPayload?: SendPaymentPayloadResolvers<ContextType>;
  SendRosettaTransactionPayload?: SendRosettaTransactionPayloadResolvers<ContextType>;
  SendTestZkappInput?: GraphQLScalarType;
  SendZkappPayload?: SendZkappPayloadResolvers<ContextType>;
  SetCoinbaseReceiverPayload?: SetCoinbaseReceiverPayloadResolvers<ContextType>;
  SetConnectionGatingConfigPayload?: SetConnectionGatingConfigPayloadResolvers<ContextType>;
  SetSnarkWorkFeePayload?: SetSnarkWorkFeePayloadResolvers<ContextType>;
  SetSnarkWorkerPayload?: SetSnarkWorkerPayloadResolvers<ContextType>;
  Sign?: GraphQLScalarType;
  Signature?: GraphQLScalarType;
  SignedFee?: SignedFeeResolvers<ContextType>;
  Slot?: GraphQLScalarType;
  SnarkWorker?: SnarkWorkerResolvers<ContextType>;
  Span?: GraphQLScalarType;
  StagedLedgerAuxHash?: GraphQLScalarType;
  StakingEpochData?: StakingEpochDataResolvers<ContextType>;
  StateHash?: GraphQLScalarType;
  StateHashAsDecimal?: GraphQLScalarType;
  TarFile?: TarFileResolvers<ContextType>;
  Time?: GraphQLScalarType;
  Timing?: TimingResolvers<ContextType>;
  TokenId?: GraphQLScalarType;
  TransactionHash?: GraphQLScalarType;
  TransactionId?: GraphQLScalarType;
  TransactionInfo?: TransactionInfoResolvers<ContextType>;
  TransactionStatusFailure?: GraphQLScalarType;
  Transactions?: TransactionsResolvers<ContextType>;
  TrustStatusPayload?: TrustStatusPayloadResolvers<ContextType>;
  UInt32?: GraphQLScalarType;
  UInt64?: GraphQLScalarType;
  UnlockPayload?: UnlockPayloadResolvers<ContextType>;
  UserCommand?: UserCommandResolvers<ContextType>;
  UserCommandDelegation?: UserCommandDelegationResolvers<ContextType>;
  UserCommandKind?: GraphQLScalarType;
  UserCommandPayment?: UserCommandPaymentResolvers<ContextType>;
  VerificationKey?: GraphQLScalarType;
  VerificationKeyHash?: GraphQLScalarType;
  VerificationKeyWithHash?: VerificationKeyWithHashResolvers<ContextType>;
  VrfEvaluation?: VrfEvaluationResolvers<ContextType>;
  VrfMessage?: VrfMessageResolvers<ContextType>;
  VrfOutputTruncated?: GraphQLScalarType;
  VrfScalar?: GraphQLScalarType;
  VrfThreshold?: VrfThresholdResolvers<ContextType>;
  WorkDescription?: WorkDescriptionResolvers<ContextType>;
  ZkappAccountUpdate?: ZkappAccountUpdateResolvers<ContextType>;
  ZkappCommand?: ZkappCommandResolvers<ContextType>;
  ZkappCommandFailureReason?: ZkappCommandFailureReasonResolvers<ContextType>;
  ZkappCommandResult?: ZkappCommandResultResolvers<ContextType>;
  ZkappFeePayer?: ZkappFeePayerResolvers<ContextType>;
  ZkappProof?: GraphQLScalarType;
  epochLedger?: EpochLedgerResolvers<ContextType>;
  mutation?: MutationResolvers<ContextType>;
  protocolStateProof?: ProtocolStateProofResolvers<ContextType>;
  query?: QueryResolvers<ContextType>;
  subscription?: SubscriptionResolvers<ContextType>;
};

