// SPDX-License-Identifier: MIT
pragma solidity 0.8.18;

import {ERC1967Upgrade} from "@openzeppelin/contracts/proxy/ERC1967/ERC1967Upgrade.sol";
import {NetworkId, signer} from "./MinaPrecompiles.sol";
import {State} from "./State.sol";
import {Transaction, MinaPublicKey, HashedMinaPublicKey, MinaSchnorrSignature} from "./Types.sol";
import {FieldBytes} from "./FieldBytes.sol";

/**
 * @title MinaMultisig
 * @dev Contains multisig functionality for the Data Availability Layer, i.e. defines
 * how validators can propose arbitrary transaction execution when confirmed
 * by a majority of them.
 *
 * @notice This contract is modified version of SidechainBridge's Multisig on C1.
 *
 * Don't define any storage variables here!
 */
abstract contract MinaMultisig is ERC1967Upgrade, State {
    using FieldBytes for bytes;

    /**
     * @dev Emitted when a validator votes for a transaction proposal
     */
    event Confirmation(HashedMinaPublicKey indexed sender, bytes32 indexed transactionId);

    /**
     * @dev Emitted when a transaction proposal gets executed successfully
     */
    event Execution(bytes32 indexed transactionId);

    /**
     * @dev Emitted when a transaction proposal gathers `quorum` of votes
     * but its execution fails
     */
    event ExecutionFailure(bytes32 indexed transactionId);

    /**
     * @dev Emitted when validators are added, removed or replaced.
     * Public key fields can be empty (equal 0) if corresponding change is not
     * performed.
     */
    event ValidatorsUpdated(
        HashedMinaPublicKey indexed removedValidator,
        HashedMinaPublicKey indexed addedValidator
    );

    /**
     * @dev Emitted when quorum or validator set is changed
     */
    event QuorumChanged(uint256 quorum);

    /**
     * @dev A central modifier implementing the "validator-majority only"
     * functionality: the bridge contract itself will call a function only
     * when `quorum` of validators have voted for it.
     */
    modifier onlyMultisig() {
        if (msg.sender != address(this)) revert("Sender is not the multisig");
        _;
    }

    modifier validatorDoesNotExist(HashedMinaPublicKey validator) {
        if (isValidator[validator]) revert("Validator exists");
        _;
    }

    modifier validatorExists(HashedMinaPublicKey validator) {
        if (!isValidator[validator]) revert("Validator doesn't exist");
        _;
    }

    modifier confirmed(bytes32 transactionId, HashedMinaPublicKey validator) {
        if (!confirmations[transactionId][validator])
            revert("Transaction not confirmed by validator");
        _;
    }

    modifier notConfirmed(bytes32 transactionId, HashedMinaPublicKey validator) {
        if (confirmations[transactionId][validator]) revert("Transaction confirmed by validator");
        _;
    }

    modifier notExecuted(bytes32 transactionId) {
        if (transactions[transactionId].executed) revert("Transaction already executed");
        _;
    }

    modifier notNull(address _address) {
        if (_address == address(0)) revert("Null address");
        _;
    }

    modifier validQuorumRequirement(uint256 validatorCount, uint256 _quorum) {
        if (_quorum > validatorCount || _quorum == 0 || validatorCount == 0)
            revert("Invalid quorum");
        if (_quorum < validatorCount / 2 + 1) revert("Quorum does not meet 51% majority");
        _;
    }

    modifier nonReentrant() {
        require(_reentrancy_status != _REENTRANCY_ENTERED, "ReentrancyGuard: reentrant call");
        _reentrancy_status = _REENTRANCY_ENTERED;
        _;
        _reentrancy_status = _REENTRANCY_NOT_ENTERED;
    }

    constructor() {
        initialized = true;
    }

    function initialize(
        uint256 quorum_,
        MinaPublicKey[] calldata validators_
    ) public validQuorumRequirement(validators_.length, quorum_) {
        require(!initialized, "Contract already initialized");
        initialized = true;

        quorum = quorum_;

        for (uint256 i = 0; i < validators_.length; i++) {
            HashedMinaPublicKey hashedValidator = hashPublicKey(validators_[i]);

            require(!isValidator[hashedValidator], "Duplicate validator");

            isValidator[hashedValidator] = true;
            validators.push(validators_[i]);
        }
    }

    /*
     * Public functions
     */

    /**
     * @dev Allows to add a new validator. Only doable by validator majority!
     * @param validator Mina public key of the new validator.
     * @param newQuorum New quorum.
     */
    function addValidator(
        MinaPublicKey calldata validator,
        uint256 newQuorum
    ) public onlyMultisig validatorDoesNotExist(hashPublicKey(validator)) {
        HashedMinaPublicKey hashedValidator = hashPublicKey(validator);

        isValidator[hashedValidator] = true;
        validators.push(validator);

        emit ValidatorsUpdated(HashedMinaPublicKey.wrap(0), hashedValidator);

        changeQuorum(newQuorum);
    }

    /**
     * @dev Allows to remove a validator. Only doable by validator majority!
     * @param validator Mina public key of the validator.
     * @param newQuorum New quorum.
     */
    function removeValidator(
        MinaPublicKey calldata validator,
        uint256 newQuorum
    ) public onlyMultisig validatorExists(hashPublicKey(validator)) {
        // Validator's votes won't be cleared as this is unnecessary.
        // Removal of votes on the finished transactions would have no effect.
        // As for unfinished proposals, we rely on the honest majority of
        // validators to continuously guarantee that proposals are being voted
        // if they are necessary. I.e. if a bad actor who was voting on invalid
        // proposals is being removed, we assume the honest majority will not
        // vote for given invalid proposals anyway or that a valid proposal
        // would have been passed without him, so his contribution is not
        // detrimental.

        HashedMinaPublicKey hashedValidator = hashPublicKey(validator);

        isValidator[hashedValidator] = false;

        for (uint256 i = 0; i < validators.length - 1; i++) {
            if (validators[i].x == validator.x && validators[i].y == validator.y) {
                validators[i] = validators[validators.length - 1];
                break;
            }
        }
        validators.pop();

        emit ValidatorsUpdated(hashedValidator, HashedMinaPublicKey.wrap(0));

        changeQuorum(newQuorum);
    }

    /**
     * @dev Allows to replace a validator with a new validator. Only doable by
     * validator majority!
     * @param oldValidator Mina public key of the validator to be replaced.
     * @param newValidator Mina public key of the validator to be added.
     */
    function replaceValidator(
        MinaPublicKey calldata oldValidator,
        MinaPublicKey calldata newValidator
    )
        public
        onlyMultisig
        validatorExists(hashPublicKey(oldValidator))
        validatorDoesNotExist(hashPublicKey(newValidator))
    {
        HashedMinaPublicKey hashedOldValidator = hashPublicKey(oldValidator);
        HashedMinaPublicKey hashedNewValidator = hashPublicKey(newValidator);

        for (uint256 i = 0; i < validators.length; i++) {
            if (validators[i].x == oldValidator.x && validators[i].y == oldValidator.y) {
                validators[i] = newValidator;
                break;
            }
        }

        isValidator[hashedOldValidator] = false;
        isValidator[hashedNewValidator] = true;

        emit ValidatorsUpdated(hashedOldValidator, hashedNewValidator);

        changeQuorum(quorum);
    }

    /**
     * @dev Allows to change the quorum. Only doable by validator majority!
     * @param quorum_ Number of votes needed to execute a transaction.
     */
    function changeQuorum(
        uint256 quorum_
    ) public onlyMultisig validQuorumRequirement(validators.length, quorum_) {
        if (quorum != quorum_) {
            quorum = quorum_;

            emit QuorumChanged(quorum_);
        }
    }

    /**
     * @dev Return address of current logic contract
     */
    function getImplementation() public view returns (address) {
        return _getImplementation();
    }

    /**
     * @dev Allows to upgrade the bridge by changing its implementation
     * address. Only doable by validator majority!
     * @param newContract New logic contract (implementation) address.
     */
    function upgradeContract(address newContract) public onlyMultisig {
        if (_getImplementation() != newContract) {
            _upgradeTo(newContract);
        }
    }

    /**
     * @dev Returns if there is a transaction proposal stored under given id.
     * @param transactionId Unique id of transaction.
     */
    function transactionExists(bytes32 transactionId) public view returns (bool) {
        return transactions[transactionId].destination != address(0);
    }

    /**
     * @dev Allows a validator to vote for a transaction. If the transactionId
     * doesn't exist yet, the transaction will be created. If the transactionId
     * exists and remaining arguments are equal to stored ones, the validator's
     * vote for the transaction is added. If any argument differs from its
     * stored counterpart, the function reverts.
     * @param transactionId Unique id of the transaction.
     * @param destination Transaction target address.
     * @param value Transaction value.
     * @param data Transaction data payload.
     * @param signature Validator's signature over the transaction.
     */
    function voteForTransaction(
        bytes32 transactionId,
        address destination,
        uint256 value,
        bytes calldata data,
        MinaSchnorrSignature calldata signature
    ) public nonReentrant validatorExists(hashPublicKey(signature.publicKey)) {
        bool signatureValid = signer.verify(
            NetworkId.TESTNET,
            signature.publicKey.x,
            signature.publicKey.y,
            signature.rx,
            signature.s,
            abi.encodePacked(transactionId, destination, value, data).toFields()
        );

        require(signatureValid, "Invalid signature");

        if (!transactionExists(transactionId))
            addTransaction(transactionId, destination, value, data);
        else {
            Transaction storage transaction = transactions[transactionId];

            require(
                block.timestamp <= transaction.validatorVotePeriod,
                "Time expired to vote validator"
            );

            require(
                transaction.destination == destination &&
                    transaction.value == value &&
                    keccak256(transaction.data) == keccak256(data),
                "Request id exists with incompatible data"
            );
        }
        confirmTransaction(transactionId, hashPublicKey(signature.publicKey));
    }

    /**
     * @dev Allows anyone to execute a confirmed transaction.
     * @param transactionId Transaction ID.
     */
    function executeTransaction(
        bytes32 transactionId
    ) public nonReentrant notExecuted(transactionId) {
        reentrantExecuteTransaction(transactionId);
    }

    /**
     * @dev Fullfills the purpose executeTransaction but without reentrancy
     * protection to be called by other public functions.
     * @param transactionId Transaction ID.
     */
    function reentrantExecuteTransaction(
        bytes32 transactionId
    ) internal notExecuted(transactionId) {
        if (isConfirmed(transactionId)) {
            Transaction storage transaction = transactions[transactionId];
            transaction.executed = true;
            uint256 value = transaction.value;
            (bool success, ) = transaction.destination.call{value: value}(transaction.data);
            if (success) {
                emit Execution(transactionId);
            } else {
                emit ExecutionFailure(transactionId);
                transaction.executed = false;
            }
        }
    }

    /**
     * @dev Returns the confirmation status of a transaction.
     * @param transactionId Transaction ID.
     * @return Confirmation status.
     */
    function isConfirmed(bytes32 transactionId) public view returns (bool) {
        uint256 count = 0;
        for (uint256 i = 0; i < validators.length; i++) {
            if (confirmations[transactionId][hashPublicKey(validators[i])]) count += 1;
            if (count == quorum) return true;
        }
        return false;
    }

    /*
     * Internal functions
     */

    /**
     * @dev Adds a new transaction to the transaction mapping, if the
     * transaction does not exist yet.
     * @param transactionId Requested id to store the transaction under.
     * @param destination Transaction target address.
     * @param value Transaction ether value.
     * @param data Transaction data payload.
     */
    function addTransaction(
        bytes32 transactionId,
        address destination,
        uint256 value,
        bytes calldata data
    ) internal notNull(destination) {
        transactions[transactionId] = Transaction({
            destination: destination,
            value: value,
            data: data,
            executed: false,
            validatorVotePeriod: block.timestamp + VALIDATOR_VOTE_PERIOD
        });

        transactionIds.push(transactionId);
    }

    /**
     * @dev Confirms a transaction by current sender.
     * @param transactionId Transaction ID.
     */
    function confirmTransaction(
        bytes32 transactionId,
        HashedMinaPublicKey validator
    ) internal notConfirmed(transactionId, validator) {
        confirmations[transactionId][validator] = true;
        emit Confirmation(validator, transactionId);
        if (!transactions[transactionId].executed) reentrantExecuteTransaction(transactionId);
    }

    /*
     * Web3 call functions
     */

    /**
     * @dev Returns number of confirmations of a transaction.
     * @param transactionId Transaction ID.
     * @return count Number of confirmations.
     */
    function getConfirmationCount(bytes32 transactionId) public view returns (uint256 count) {
        for (uint256 i = 0; i < validators.length; i++)
            if (confirmations[transactionId][hashPublicKey(validators[i])]) count += 1;
    }

    /**
     * @dev Returns total number of transactions after filters are applied.
     * @param pending Include pending transactions.
     * @param executed Include executed transactions.
     * @return count Total number of transactions after filters are applied.
     */
    function getTransactionCount(bool pending, bool executed) public view returns (uint256 count) {
        for (uint256 i = 0; i < transactionIds.length; i++) {
            bool txExecuted = transactions[transactionIds[i]].executed;
            if ((!txExecuted && pending) || (txExecuted && executed)) count += 1;
        }
    }

    /**
     * @dev Returns list of validators.
     * @return List of validator addresses.
     */
    function getValidators() public view returns (MinaPublicKey[] memory) {
        return validators;
    }

    /**
     * @dev Returns array with validator public keys, which confirmed
     * the transaction.
     * @param transactionId Transaction ID.
     * @return confirmations_ Returns array of validator addresses.
     */
    function getConfirmations(
        bytes32 transactionId
    ) public view returns (MinaPublicKey[] memory confirmations_) {
        MinaPublicKey[] memory confirmationsTemp = new MinaPublicKey[](validators.length);

        uint256 count = 0;
        uint256 i;

        for (i = 0; i < validators.length; i++) {
            if (confirmations[transactionId][hashPublicKey(validators[i])]) {
                confirmationsTemp[count] = validators[i];
                count += 1;
            }
        }

        confirmations_ = new MinaPublicKey[](count);
        for (i = 0; i < count; i++) confirmations_[i] = confirmationsTemp[i];
    }

    /**
     * @dev Returns list of transaction IDs in defined range.
     * @param from Index start position of transaction array.
     * @param to Index end position of transaction array (non-inclusive).
     * @param pending Include pending transactions.
     * @param executed Include executed transactions.
     * @return transactionIds_ Returns array of transaction IDs.
     */
    function getTransactionIds(
        uint256 from,
        uint256 to,
        bool pending,
        bool executed
    ) public view returns (bytes32[] memory transactionIds_) {
        if (to > transactionIds.length) to = transactionIds.length;
        if (from > to) from = to;
        bytes32[] memory transactionIdsTemp = new bytes32[](to - from);
        uint256 count = 0;
        for (uint256 i = from; i < to; i++) {
            bool txExecuted = transactions[transactionIds[i]].executed;
            if ((!txExecuted && pending) || (txExecuted && executed)) {
                transactionIdsTemp[count] = transactionIds[i];
                count += 1;
            }
        }
        transactionIds_ = new bytes32[](count);
        for (uint256 i = 0; i < count; i++) transactionIds_[i] = transactionIdsTemp[i];
    }

    function hashPublicKey(
        MinaPublicKey memory publicKey
    ) internal pure returns (HashedMinaPublicKey) {
        return HashedMinaPublicKey.wrap(keccak256(abi.encodePacked(publicKey.x, publicKey.y)));
    }
}
