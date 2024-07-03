// SPDX-License-Identifier: MIT
pragma solidity >=0.8.13;

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

/**
 * @dev Mina specific types
 */

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

struct RollupBatch {
    string data;
    MinaSchnorrSignature[] signatures;
    // sigData corresponds to the target ledger hash that validator signs
    // this is for the DA layer to verify the signature
    bytes32[] sigData;
}
