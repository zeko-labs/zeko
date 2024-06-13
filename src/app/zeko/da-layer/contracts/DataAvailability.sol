// SPDX-License-Identifier: MIT
pragma solidity >=0.8.13;

import {hasher, signer, NetworkId} from "./MinaPrecompiles.sol";
import {MinaSchnorrSignature, MinaPublicKey, HashedMinaPublicKey, RollupBatch} from "./Types.sol";
import {MinaMultisig} from "./MinaMultisig.sol";
import {FieldBytes} from "./FieldBytes.sol";
import {Endianness} from "./Endianness.sol";

contract DataAvailability is MinaMultisig {
    using FieldBytes for bytes;
    using Endianness for bytes32;

    event BatchPosted(uint256 indexed location);
    event BatchSigned(
        uint256 indexed location,
        HashedMinaPublicKey indexed publicKey,
        uint256 signatureCount
    );

    modifier onlySequencer() {
        require(msg.sender == sequencer, "Only sequencer can call this function");
        _;
    }

    function setSequencer(address sequencer_) external onlyMultisig {
        sequencer = sequencer_;
    }

    function postBatch(
        int256 previousLocation,
        bytes memory sourceReceiptChainHashes,
        bytes[] memory commands,
        bytes memory targetSparseLedger,
        bytes32[] memory sigData
    ) external onlySequencer {
        require(commands.length > 0, "Commands cannot be empty");
        // -1 means genesis batch
        require(
            previousLocation < (int256)(batchesLength) && previousLocation >= -1,
            "Invalid previousLocation"
        );

        RollupBatch storage batch = batches[batchesLength];

        batch.previousLocation = previousLocation;
        batch.sourceReceiptChainHashes = sourceReceiptChainHashes;
        batch.commands = commands;
        batch.targetSparseLedger = targetSparseLedger;
        batch.sigData = sigData;

        emit BatchPosted(batchesLength);

        batchesLength++;
    }

    function addBatchSignature(
        uint256 location,
        MinaSchnorrSignature calldata signature
    ) external validatorExists(hashPublicKey(signature.publicKey)) {
        RollupBatch storage batch = batches[location];

        require(
            !validatorSigned[hashPublicKey(signature.publicKey)][location],
            "Validator already signed"
        );

        bool verified = signer.verify(
            NetworkId.TESTNET,
            signature.publicKey.x,
            signature.publicKey.y,
            signature.rx,
            signature.s,
            batch.sigData
        );

        require(verified, "Invalid signature");

        batch.signatures.push(signature);
        validatorSigned[hashPublicKey(signature.publicKey)][location] = true;

        emit BatchSigned(location, hashPublicKey(signature.publicKey), batch.signatures.length);
    }

    function getBatchSignatures(
        uint256 location
    ) external view returns (MinaSchnorrSignature[] memory) {
        return batches[location].signatures;
    }

    function getBatchData(
        uint256 location
    )
        external
        view
        returns (int256, bytes memory, bytes[] memory, bytes memory, bytes32[] memory)
    {
        RollupBatch storage batch = batches[location];
        return (
            batch.previousLocation,
            batch.sourceReceiptChainHashes,
            batch.commands,
            batch.targetSparseLedger,
            batch.sigData
        );
    }
}
