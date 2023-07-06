// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {hasher, signer, NetworkId} from "./MinaPrecompiles.sol";
import {MinaSchnorrSignature, MinaPublicKey, HashedMinaPublicKey, RollupBatch, MinaCommand} from "./Types.sol";
import {MinaMultisig} from "./MinaMultisig.sol";
import {FieldBytes} from "./FieldBytes.sol";

contract DataAvailability is MinaMultisig {
    using FieldBytes for bytes;

    // TODO: for testing, this should be fetched from L1 contract
    bytes32 public lastProposedBatch;

    event BatchProposed(bytes32 indexed ledgerHash);
    event BatchSigned(
        bytes32 indexed ledgerHash,
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

    function proposeBatch(
        bytes32 ledgerHash,
        bytes32 previousLedgerHash,
        MinaCommand[] memory commands
    ) external onlySequencer {
        require(commands.length > 0, "Fields cannot be empty");
        require(ledgerHash != previousLedgerHash, "Batch cannot be equal to previous batch");
        require(
            batches[previousLedgerHash].commands.length > 0 || previousLedgerHash == bytes32(0),
            "Previous batch does not exist"
        );

        RollupBatch storage batch = batches[ledgerHash];

        require(batch.commands.length == 0, "Batch already exists");

        batch.previousLedgerHash = previousLedgerHash;

        for (uint256 i = 0; i < commands.length; i++) {
            batch.commands.push(commands[i]);
        }

        emit BatchProposed(ledgerHash);
        lastProposedBatch = ledgerHash;
    }

    function addBatchSignature(
        bytes32 ledgerHash,
        bytes32[] calldata sigData,
        MinaSchnorrSignature calldata signature
    ) external validatorExists(hashPublicKey(signature.publicKey)) {
        RollupBatch storage batch = batches[ledgerHash];

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
            sigData
        );

        require(verified, "Invalid signature");

        batch.signatures.push(signature);
        batch.validatorSigned[hashPublicKey(signature.publicKey)] = true;

        emit BatchSigned(ledgerHash, hashPublicKey(signature.publicKey), batch.signatures.length);
    }

    function getBatchSignatures(
        bytes32 ledgerHash
    ) external view returns (MinaSchnorrSignature[] memory) {
        return batches[ledgerHash].signatures;
    }

    function getBatchData(
        bytes32 ledgerHash
    ) external view returns (bytes32, MinaCommand[] memory) {
        return (batches[ledgerHash].previousLedgerHash, batches[ledgerHash].commands);
    }

    // function getBatchFields(bytes32 ledgerHash) external view returns (bytes32[] memory) {
    //     require(batches[ledgerHash].commands.length > 0, "Batch does not exist");

    //     return batchToFields(batches[ledgerHash].previousLedgerHash, batches[ledgerHash].commands);
    // }

    // function batchToFields(
    //     bytes32 ledgerHash,
    //     bytes32 previousLedgerHash,
    //     MinaCommand[] memory commands
    // ) private pure returns (bytes32[] memory) {
    //     bytes memory data = abi.encodePacked(ledgerHash, previousLedgerHash);

    //     for (uint256 i = 0; i < commands.length; i++)
    //         data = bytes.concat(data, bytes1(uint8(commands[i].commandType)), commands[i].data);

    //     return data.toFields();
    // }
}
