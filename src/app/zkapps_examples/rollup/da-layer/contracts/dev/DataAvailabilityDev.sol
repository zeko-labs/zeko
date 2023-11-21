// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {DataAvailability} from "../DataAvailability.sol";

contract DataAvailabilityDev is DataAvailability {
    /**
     * @dev modify transaction validator vote period for testing validator
     * adding on testnet without lengthy waiting period
     */
    function setTransactionValidatorVotePeriodDev(bytes32 transactionId, uint256 newValue) public {
        transactions[transactionId].validatorVotePeriod = newValue;
    }

    function setQuorumDev(uint256 newQuorum) public {
        quorum = newQuorum;
    }
}
