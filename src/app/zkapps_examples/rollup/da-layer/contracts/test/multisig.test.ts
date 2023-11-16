import { expect } from "chai";
import { ethers } from "hardhat";
import { isReady as isSnarkyjsReady, PrivateKey, Signature } from "snarkyjs";
import { bytesToFields, fieldToHex, hashPublicKey, signatureToStruct } from "../utils/mina";
import { deployDataAvailabilityContract } from "./fixtures";

describe("Multisig", () => {
  before(async () => {
    await isSnarkyjsReady;
  });

  it("should remove validator", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      3
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("removeValidator", [
      {
        x: fieldToHex(validators[0].toPublicKey().toGroup().x),
        y: fieldToHex(validators[0].toPublicKey().toGroup().y),
      },
      3,
    ]);

    const txId = ethers.utils.randomBytes(32);

    for (const validator of validators.slice(1)) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();
    }

    expect((await dataAvailabilityContract.getValidators()).length).to.equal(4);
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(validators[0].toPublicKey())))
      .to.be.false;
  });

  it("should add validator", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      3
    );

    const newValidator = PrivateKey.random();

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("addValidator", [
      {
        x: fieldToHex(newValidator.toPublicKey().toGroup().x),
        y: fieldToHex(newValidator.toPublicKey().toGroup().y),
      },
      4,
    ]);

    const txId = ethers.utils.randomBytes(32);

    for (const validator of validators) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();
    }

    expect((await dataAvailabilityContract.getValidators()).length).to.equal(6);
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(newValidator.toPublicKey())))
      .to.be.true;
  });

  it("should not add validator again", async () => {
    const validator = PrivateKey.random();

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      [validator.toPublicKey()],
      1
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("addValidator", [
      {
        x: fieldToHex(validator.toPublicKey().toGroup().x),
        y: fieldToHex(validator.toPublicKey().toGroup().y),
      },
      2,
    ]);

    const txId = ethers.utils.randomBytes(32);

    const signature = Signature.create(
      validator,
      bytesToFields(
        ethers.utils.solidityPack(
          ["bytes32", "address", "uint256", "bytes"],
          [txId, dataAvailabilityContract.address, 0, calldata]
        )
      )
    );

    await expect(
      dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, validator.toPublicKey())
      )
    ).to.emit(dataAvailabilityContract, "ExecutionFailure");
  });

  it("should replace validator", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      3
    );

    const newValidator = PrivateKey.random();

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("replaceValidator", [
      {
        x: fieldToHex(validators[0].toPublicKey().toGroup().x),
        y: fieldToHex(validators[0].toPublicKey().toGroup().y),
      },
      {
        x: fieldToHex(newValidator.toPublicKey().toGroup().x),
        y: fieldToHex(newValidator.toPublicKey().toGroup().y),
      },
    ]);

    const txId = ethers.utils.randomBytes(32);

    for (const validator of validators.slice(1)) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();
    }

    expect((await dataAvailabilityContract.getValidators()).length).to.equal(5);
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(validators[0].toPublicKey())))
      .to.be.false;
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(newValidator.toPublicKey())))
      .to.be.true;
  });

  it("should change quorum", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      3
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("changeQuorum", [5]);

    const txId = ethers.utils.randomBytes(32);

    for (const validator of validators.slice(0, 3)) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();
    }

    expect(await dataAvailabilityContract.quorum()).to.equal(5);
  });

  it("should not allow non-validator submit transaction", async () => {
    const validators = Array.from({ length: 5 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      3
    );

    const nonValidator = PrivateKey.random();

    const txId = ethers.utils.randomBytes(32);
    const calldata = dataAvailabilityContract.interface.encodeFunctionData("changeQuorum", [5]);

    const signature = Signature.create(
      nonValidator,
      bytesToFields(
        ethers.utils.solidityPack(
          ["bytes32", "address", "uint256", "bytes"],
          [txId, dataAvailabilityContract.address, 0, calldata]
        )
      )
    );

    await expect(
      dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, nonValidator.toPublicKey())
      )
    ).to.be.revertedWith("Validator doesn't exist");
  });

  it("should not allow non-validator vote on transaction", async () => {
    const validator = PrivateKey.random();
    const nonValidator = PrivateKey.random();

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      [validator.toPublicKey()],
      1
    );

    const txId = ethers.utils.randomBytes(32);
    const calldata = dataAvailabilityContract.interface.encodeFunctionData("changeQuorum", [5]);

    const signature = Signature.create(
      validator,
      bytesToFields(
        ethers.utils.solidityPack(
          ["bytes32", "address", "uint256", "bytes"],
          [txId, dataAvailabilityContract.address, 0, calldata]
        )
      )
    );

    await (
      await dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, validator.toPublicKey())
      )
    ).wait();

    const signature2 = Signature.create(
      nonValidator,
      bytesToFields(
        ethers.utils.solidityPack(
          ["bytes32", "address", "uint256", "bytes"],
          [txId, dataAvailabilityContract.address, 0, calldata]
        )
      )
    );

    await expect(
      dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature2, nonValidator.toPublicKey())
      )
    ).to.be.revertedWith("Validator doesn't exist");
  });

  it("should not allow validator submit transaction vote after vote period", async () => {
    const [validator1, validator2] = Array.from({ length: 2 }, () => PrivateKey.random());

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      [validator1.toPublicKey(), validator2.toPublicKey()],
      2
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("changeQuorum", [5]);
    const txId = ethers.utils.randomBytes(32);

    const signature1 = Signature.create(
      validator1,
      bytesToFields(
        ethers.utils.solidityPack(
          ["bytes32", "address", "uint256", "bytes"],
          [txId, dataAvailabilityContract.address, 0, calldata]
        )
      )
    );

    // Submit transaction
    await (
      await dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature1, validator1.toPublicKey())
      )
    ).wait();

    // Time travel
    await (
      await dataAvailabilityContract.setTransactionValidatorVotePeriodDev(
        txId,
        (await ethers.provider.getBlock("latest")).timestamp - 1
      )
    ).wait();

    // Submit transaction vote
    const signature2 = Signature.create(
      validator2,
      bytesToFields(
        ethers.utils.solidityPack(
          ["bytes32", "address", "uint256", "bytes"],
          [txId, dataAvailabilityContract.address, 0, calldata]
        )
      )
    );

    await expect(
      dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature2, validator2.toPublicKey())
      )
    ).to.be.revertedWith("Transaction vote period has ended");
  });

  it("should not execute transaction if quorum is not reached", async () => {
    const validators = Array.from({ length: 3 }, () => PrivateKey.random());

    const originalQuorum = 3;
    const newQuorum = 2;

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      originalQuorum
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("changeQuorum", [
      newQuorum,
    ]);
    const txId = ethers.utils.randomBytes(32);

    for (const validator of validators.slice(0, 2)) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();
    }

    expect(await dataAvailabilityContract.isConfirmed(txId)).to.be.false;
    await (await dataAvailabilityContract.executeTransaction(txId)).wait();

    expect(await dataAvailabilityContract.quorum()).to.equal(originalQuorum);
    expect((await dataAvailabilityContract.transactions(txId)).executed).to.be.false;
  });

  it("should execute transaction using executeTransaction after quorum change", async () => {
    const validators = Array.from({ length: 3 }, () => PrivateKey.random());
    const newValidator = PrivateKey.random();

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      3
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("addValidator", [
      {
        x: fieldToHex(newValidator.toPublicKey().toGroup().x),
        y: fieldToHex(newValidator.toPublicKey().toGroup().y),
      },
      3,
    ]);
    const txId = ethers.utils.randomBytes(32);

    // not enough votes
    for (const validator of validators.slice(0, 2)) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();
    }

    expect(await dataAvailabilityContract.isConfirmed(txId)).to.be.false;
    expect((await dataAvailabilityContract.transactions(txId)).executed).to.be.false;
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(newValidator.toPublicKey())))
      .to.be.false;

    // change quorum and try execute
    await (await dataAvailabilityContract.setQuorumDev(2)).wait();
    expect(await dataAvailabilityContract.isConfirmed(txId)).to.be.true;

    await (await dataAvailabilityContract.executeTransaction(txId)).wait();

    expect((await dataAvailabilityContract.transactions(txId)).executed).to.be.true;
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(newValidator.toPublicKey())))
      .to.be.true;
  });

  it("should return valid state of votes", async () => {
    const validators = Array.from({ length: 10 }, () => PrivateKey.random());
    const quorum = 6;

    const dataAvailabilityContract = await deployDataAvailabilityContract(
      validators.map((validator) => validator.toPublicKey()),
      quorum
    );

    const calldata = dataAvailabilityContract.interface.encodeFunctionData("changeQuorum", [7]);
    const txId = ethers.utils.randomBytes(32);

    let counter = 0;
    for (const validator of validators.slice(0, 5)) {
      const signature = Signature.create(
        validator,
        bytesToFields(
          ethers.utils.solidityPack(
            ["bytes32", "address", "uint256", "bytes"],
            [txId, dataAvailabilityContract.address, 0, calldata]
          )
        )
      );

      await (
        await dataAvailabilityContract.voteForTransaction(
          txId,
          dataAvailabilityContract.address,
          0,
          calldata,
          signatureToStruct(signature, validator.toPublicKey())
        )
      ).wait();

      counter++;
      expect(await dataAvailabilityContract.getConfirmationCount(txId)).to.equal(counter);

      if (counter >= quorum) {
        expect(await dataAvailabilityContract.isConfirmed(txId)).to.be.true;
      } else {
        expect(await dataAvailabilityContract.isConfirmed(txId)).to.be.false;
      }
    }
  });
});
