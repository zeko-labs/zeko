// SPDX-License-Identifier: MIT
pragma solidity >=0.8.13;

import {hasher, signer, NetworkId} from "./MinaPrecompiles.sol";
import {MinaSchnorrSignature, MinaPublicKey, HashedMinaPublicKey, RollupBatch, MinaCommand} from "./Types.sol";
import {MinaMultisig} from "./MinaMultisig.sol";
import {FieldBytes} from "./FieldBytes.sol";
import {Endianness} from "./Endianness.sol";

contract DataAvailability is MinaMultisig {
    using FieldBytes for bytes;
    using Endianness for bytes32;

    event BatchPosted(bytes32 indexed id);
    event BatchSigned(
        bytes32 indexed id,
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
        bytes32 id,
        bytes32 previousId,
        MinaCommand[] memory commands
    ) external onlySequencer {
        require(commands.length > 0, "Commands cannot be empty");
        require(id != previousId, "Batch cannot be equal to previous batch");

        RollupBatch storage batch = batches[id];

        require(batch.commands.length == 0, "Batch already exists");

        batch.previousId = previousId;

        if (batches[previousId].commands.length == 0 || previousId == bytes32(0)) {
            batch.genesis = true;
        } else {
            batch.genesis = false;
        }

        for (uint256 i = 0; i < commands.length; i++) {
            batch.commands.push(commands[i]);
        }

        lastBatchId = id;
        emit BatchPosted(id);
    }

    function addBatchSignature(
        bytes32 id,
        MinaSchnorrSignature calldata signature
    ) external validatorExists(hashPublicKey(signature.publicKey)) {
        RollupBatch storage batch = batches[id];

        require(
            !batch.validatorSigned[hashPublicKey(signature.publicKey)],
            "Validator already signed"
        );

        bytes32[] memory sigData = new bytes32[](1);
        sigData[0] = id;

        bool verified = signer.verify(
            NetworkId.TESTNET,
            signature.publicKey.x,
            signature.publicKey.y,
            signature.rx,
            signature.s,
            sigData
        );

        require(verified, "Invalid signature");

        batch.signatures.push(signature);
        batch.validatorSigned[hashPublicKey(signature.publicKey)] = true;

        emit BatchSigned(id, hashPublicKey(signature.publicKey), batch.signatures.length);
    }

    function getBatchSignatures(bytes32 id) external view returns (MinaSchnorrSignature[] memory) {
        return batches[id].signatures;
    }

    function getBatchData(bytes32 id) external view returns (bool, bytes32, MinaCommand[] memory) {
        return (batches[id].genesis, batches[id].previousId, batches[id].commands);
    }
}
