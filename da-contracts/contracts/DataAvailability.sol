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

    event CommandPosted(uint256 indexed index, MinaCommand command);
    event CommandSigned(
        uint256 indexed index,
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

    function postCommand(MinaCommand memory command) external onlySequencer {
        commands.push(command);
        emit CommandPosted(commands.length - 1, command);
    }

    function addCommandSignature(
        uint256 commandIndex,
        bytes32 commandCommitment,
        MinaSchnorrSignature calldata signature
    ) external validatorExists(hashPublicKey(signature.publicKey)) {
        require(
            !validatorSignedCommand[commandIndex][hashPublicKey(signature.publicKey)],
            "Validator already signed"
        );

        bytes32[] memory sigData = new bytes32[](1);
        sigData[0] = commandCommitment;

        bool verified = signer.verify(
            NetworkId.TESTNET,
            signature.publicKey.x,
            signature.publicKey.y,
            signature.rx,
            signature.s,
            sigData
        );

        require(verified, "Invalid signature");

        commandSignatures[commandIndex].push(signature);
        validatorSignedCommand[commandIndex][hashPublicKey(signature.publicKey)] = true;

        emit CommandSigned(
            commandIndex,
            hashPublicKey(signature.publicKey),
            commandSignatures[commandIndex].length
        );
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

    function getCommandSignatures(
        uint256 commandIndex
    ) external view returns (MinaSchnorrSignature[] memory) {
        return commandSignatures[commandIndex];
    }

    function getCommandData(uint256 commandIndex) external view returns (MinaCommand memory) {
        return commands[commandIndex];
    }
}
