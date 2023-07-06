// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {Transaction, MinaPublicKey, HashedMinaPublicKey, RollupBatch} from "./Types.sol";

/**
 * @title State
 * @dev Source file that keeps all state definitions for the SidechainBridge.
 *
 * Defining all state in single source file eases writing storage-compatible
 * updates to the SidechainBridge contract and deploying them via the proxy
 * pattern.
 */
abstract contract State {
    /**
     * @dev Initialization logic of this contract needs to exist in a custom
     * initialization function instead of a constructor, as the contract is
     * used via a proxy pattern. This variable keeps track of whether we
     * already initialized, to forbid doing it more than once.
     */
    bool internal initialized;

    /**
     * @dev Time period in which validators can vote on a transaction
     */
    uint256 public constant VALIDATOR_VOTE_PERIOD = 1 weeks;

    /**
     * @dev Current set of validators, i.e. addresses that control the
     * contract.
     */
    MinaPublicKey[] public validators;
    /**
     * @dev Mapping kept in sync with validator list for fast lookups.
     */
    mapping(HashedMinaPublicKey => bool) public isValidator;

    /**
     * @dev Sidechain transaction proposals. The bridge contract will execute
     * any transaction if it gathers `quorum` of validator votes.
     */
    mapping(bytes32 => Transaction) public transactions;
    /**
     * @dev List kept in sync to not lose information on mapping keys.
     */
    bytes32[] public transactionIds;

    /**
     * @dev Mapping to keep track of validator votes for a transaction
     * proposal.
     */
    mapping(bytes32 => mapping(HashedMinaPublicKey => bool)) public confirmations;

    /**
     * @dev Number of validator votes needed to execute a validator-majority
     * only action.
     */
    uint256 public quorum;

    /**
     * @dev Magic value different from 0
     */
    uint256 internal constant _REENTRANCY_NOT_ENTERED = 1;
    uint256 internal constant _REENTRANCY_ENTERED = 2;
    /**
     * @dev We don't want to inherit from OpenZeppelin's ReentrancyGuard to
     * have all the storage variables in a single place.
     *
     * @dev Note that this is not initialized to `_REENTRANCY_NOT_ENTERED`
     * and won't be until the first call to `nonReentrant`.
     */
    uint256 internal _reentrancy_status;

    /**
     * @dev Mapping of batchId to RollupBatch, where batchId is the Poseidon
     * hash of the batch fields.
     */
    mapping(bytes32 => RollupBatch) internal batches;

    address public sequencer;
}
