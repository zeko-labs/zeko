// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

library FieldBytes {
    uint256 constant FIELD_BIT_CAPACITY = 254;
    uint256 constant FIELD_BYTE_CAPACITY = FIELD_BIT_CAPACITY / 8;

    function toFields(bytes memory data) internal pure returns (bytes32[] memory) {
        uint256 numFields = (data.length + FIELD_BYTE_CAPACITY - 1) / FIELD_BYTE_CAPACITY;

        bytes32[] memory fields = new bytes32[](numFields);

        for (uint256 i = 0; i < data.length; i++) {
            uint256 fieldIndex = i / FIELD_BYTE_CAPACITY;
            uint256 byteIndex = i % FIELD_BYTE_CAPACITY;

            fields[fieldIndex] |= bytes32(data[i]) >> (byteIndex * 8);
        }

        return fields;
    }
}
