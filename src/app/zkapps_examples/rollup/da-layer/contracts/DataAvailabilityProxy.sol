// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {ERC1967Proxy} from "@openzeppelin/contracts/proxy/ERC1967/ERC1967Proxy.sol";
import {MinaPublicKey} from "./Types.sol";

/**
 * @title Proxy
 * @dev This contract serves as a proxy that delegates all calls
 * to the TokenMerger implementation contract.
 *
 * Address of the proxy is also the owner of all the tokens available for conversion.
 */
contract DataAvailabilityProxy is ERC1967Proxy {
    constructor(
        address implementation,
        uint256 quorum,
        MinaPublicKey[] memory validators,
        address sequencer
    )
        ERC1967Proxy(
            implementation,
            abi.encodeWithSignature(
                "initialize(uint256,(bytes32,bytes32)[],address)",
                quorum,
                validators,
                sequencer
            )
        )
    {}
}
