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

    modifier postGenesis() {
        require(genesisInitialized, "First init the genesis state");
        _;
    }

    function initGenesisState(string memory genesisState_) external {
        require(!genesisInitialized, "Genesis state already initialized");

        genesisState = genesisState_;
        genesisInitialized = true;

        // We set 0th batch as genesis state
        batches[0].data = genesisState_;
        emit BatchPosted(batchesLength);
        batchesLength++;
    }

    function postBatch(
        string memory batchData,
        bytes32[] memory sigDataWithoutLocation
    ) external postGenesis {
        RollupBatch storage batch = batches[batchesLength];

        bytes32[] memory sigData = new bytes32[](sigDataWithoutLocation.length + 1);

        // Add location to the signature data
        sigData[0] = bytes32(batchesLength).swapEndianness();
        for (uint256 i = 0; i < sigDataWithoutLocation.length; i++) {
            sigData[i + 1] = sigDataWithoutLocation[i];
        }

        batch.data = batchData;
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
    ) external view returns (string memory, bytes32[] memory) {
        // Location 0 is reserved for the genesis state
        if (location == 0) {
            return ("", new bytes32[](0));
        }

        require(location < batchesLength, "Invalid location");

        RollupBatch storage batch = batches[location];
        return (batch.data, batch.sigData);
    }
}
