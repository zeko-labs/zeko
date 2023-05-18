// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

enum NetworkId {
    MAINNET,
    TESTNET,
    NULLNET
}

address constant HASHER_ADDRESS = address(0x50);
address constant SIGNER_ADDRESS = address(0x51);

interface IHasher {
    function poseidonHash(
        NetworkId networkId,
        bytes32[] memory fields
    ) external view returns (bytes32);
}

IHasher constant hasher = IHasher(HASHER_ADDRESS);

interface ISigner {
    function verify(
        NetworkId networkId,
        bytes32 pubKeyX,
        bytes32 pubKeyY,
        bytes32 signatureRX,
        bytes32 signatureS,
        bytes32[] calldata fields
    ) external view returns (bool);
}

ISigner constant signer = ISigner(SIGNER_ADDRESS);
