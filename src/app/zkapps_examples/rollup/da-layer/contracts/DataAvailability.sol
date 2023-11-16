// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {hasher, signer, NetworkId} from "./MinaPrecompiles.sol";
import {MinaSchnorrSignature, MinaPublicKey, HashedMinaPublicKey, RollupBatch, MinaCommand} from "./Types.sol";
import {MinaMultisig} from "./MinaMultisig.sol";
import {FieldBytes} from "./FieldBytes.sol";
import {Endianness} from "./Endianness.sol";

contract DataAvailability is MinaMultisig {
    using FieldBytes for bytes;
    using Endianness for bytes32;

    event CommandPosted(uint256 indexed index, MinaCommand command);
    event CommandSigned(
        uint256 indexed index,
        HashedMinaPublicKey indexed publicKey,
        uint256 signatureCount
    );

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

    function postBatch(
        bytes32 id,
        bytes32 previousId,
        uint256[] memory commands
    ) external onlySequencer {
        require(commands.length > 0, "Fields cannot be empty");
        require(id != previousId, "Batch cannot be equal to previous batch");
        require(
            batches[previousId].commands.length > 0 || previousId == bytes32(0),
            "Previous batch does not exist"
        );

        RollupBatch storage batch = batches[id];

        require(batch.commands.length == 0, "Batch already exists");

        batch.previousId = previousId;

        for (uint256 i = 0; i < commands.length; i++) {
            batch.commands.push(commands[i]);
        }

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

        bytes32[] memory sigData = new bytes32[](batch.commands.length);

        for (uint256 i = 0; i < batch.commands.length; i++) {
            sigData[i] = (bytes32(batch.commands[i])).swapEndianness();
        }

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

    function getBatchData(bytes32 id) external view returns (bytes32, uint256[] memory) {
        return (batches[id].previousId, batches[id].commands);
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
