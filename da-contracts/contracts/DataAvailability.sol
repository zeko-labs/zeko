// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {hasher, signer, NetworkId} from "./MinaPrecompiles.sol";
import {MinaSchnorrSignature, MinaPublicKey, HashedMinaPublicKey, RollupBatch, MinaCommand} from "./Types.sol";
import {MinaMultisig} from "./MinaMultisig.sol";
import {FieldBytes} from "./FieldBytes.sol";

contract DataAvailability is MinaMultisig {
    using FieldBytes for bytes;

    // TODO: for testing, this should be fetched from L1 contract
    bytes32 public lastProposedBatchId;

    event BatchProposed(bytes32 indexed batchId);
    event BatchSigned(
        bytes32 indexed batchId,
        HashedMinaPublicKey indexed publicKey,
        uint256 signatureCount
    );

    function proposeBatch(bytes32 previousBatchId, MinaCommand[] memory commands) external {
        require(commands.length > 0, "Fields cannot be empty");

        bytes32[] memory fields = batchToFields(previousBatchId, commands);

        bytes32 batchId = hasher.poseidonHash(NetworkId.NULLNET, fields);

        require(batchId != previousBatchId, "Batch cannot be equal to previous batch");

        RollupBatch storage batch = batches[batchId];

        require(batch.commands.length == 0, "Batch already exists");

        batch.previousBatchId = previousBatchId;

        for (uint256 i = 0; i < commands.length; i++) {
            batch.commands.push(commands[i]);
        }

        emit BatchProposed(batchId);
        lastProposedBatchId = batchId;
    }

    function addBatchSignature(
        bytes32 batchId,
        MinaSchnorrSignature calldata signature
    ) external validatorExists(hashPublicKey(signature.publicKey)) {
        RollupBatch storage batch = batches[batchId];

        require(
            !batch.validatorSigned[hashPublicKey(signature.publicKey)],
            "Validator already signed"
        );

        MinaCommand[] memory commands = batch.commands;

        bool verified = signer.verify(
            NetworkId.TESTNET,
            signature.publicKey.x,
            signature.publicKey.y,
            signature.rx,
            signature.s,
            batchToFields(batch.previousBatchId, commands)
        );

        require(verified, "Invalid signature");

        batch.signatures.push(signature);
        batch.validatorSigned[hashPublicKey(signature.publicKey)] = true;

        emit BatchSigned(batchId, hashPublicKey(signature.publicKey), batch.signatures.length);
    }

    function getBatchSignatures(
        bytes32 batchId
    ) external view returns (MinaSchnorrSignature[] memory) {
        return batches[batchId].signatures;
    }

    function getBatchData(bytes32 batchId) external view returns (bytes32, MinaCommand[] memory) {
        return (batches[batchId].previousBatchId, batches[batchId].commands);
    }

    function getBatchFields(bytes32 batchId) external view returns (bytes32[] memory) {
        require(batches[batchId].commands.length > 0, "Batch does not exist");

        return batchToFields(batches[batchId].previousBatchId, batches[batchId].commands);
    }

    function batchToFields(
        bytes32 previousBatchId,
        MinaCommand[] memory commands
    ) private pure returns (bytes32[] memory) {
        bytes memory data = abi.encodePacked(previousBatchId);

        for (uint256 i = 0; i < commands.length; i++)
            data = bytes.concat(data, bytes1(uint8(commands[i].commandType)), commands[i].data);

        return data.toFields();
    }
}
