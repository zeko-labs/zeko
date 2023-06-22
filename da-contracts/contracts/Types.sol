// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

struct MinaPublicKey {
    bytes32 x;
    bytes32 y;
}

type HashedMinaPublicKey is bytes32;

struct MinaSchnorrSignature {
    MinaPublicKey publicKey;
    bytes32 rx;
    bytes32 s;
}

enum MinaCommandType {
    PAYMENT,
    ZKAPP
}

struct MinaCommand {
    MinaCommandType commandType;
    bytes data;
}

struct RollupBatch {
    bytes32 previousBatchId;
    MinaCommand[] commands;
    MinaSchnorrSignature[] signatures;
    mapping(HashedMinaPublicKey => bool) validatorSigned;
}

/**
 * @dev Multisig transaction proposal, for validators to vote on and
 * execute.
 */
struct Transaction {
    address destination;
    uint256 value;
    bytes data;
    bool executed;
    uint256 validatorVotePeriod;
}
