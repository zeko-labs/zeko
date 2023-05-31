// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {hasher, signer, NetworkId} from "./MinaPrecompiles.sol";
import {MinaSchnorrSignature, MinaPublicKey, HashedMinaPublicKey, RollupBatch} from "./Types.sol";
import {MinaMultisig} from "./MinaMultisig.sol";

contract DataAvailability is MinaMultisig {
    event BatchProposed(bytes32 indexed batchId);
    event BatchSigned(
        bytes32 indexed batchId,
        HashedMinaPublicKey indexed publicKey,
        uint256 signatureCount
    );

    function proposeBatch(bytes32[] calldata fields) external {
        require(fields.length > 0, "Fields cannot be empty");

        bytes32 batchId = hasher.poseidonHash(NetworkId.NULLNET, fields);

        require(batches[batchId].fields.length == 0, "Batch already exists");

        batches[batchId].fields = fields;

        emit BatchProposed(batchId);
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

        bool verified = signer.verify(
            NetworkId.TESTNET,
            signature.publicKey.x,
            signature.publicKey.y,
            signature.rx,
            signature.s,
            batch.fields
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

    function getBatchData(bytes32 batchId) external view returns (bytes32[] memory) {
        return batches[batchId].fields;
    }
}
