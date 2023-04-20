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

      const tx = await dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, validator.toPublicKey())
      );

      await tx.wait();
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

      const tx = await dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, validator.toPublicKey())
      );

      await tx.wait();
    }

    expect((await dataAvailabilityContract.getValidators()).length).to.equal(6);
    expect(await dataAvailabilityContract.isValidator(hashPublicKey(newValidator.toPublicKey())))
      .to.be.true;
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

      const tx = await dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, validator.toPublicKey())
      );

      await tx.wait();
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

      const tx = await dataAvailabilityContract.voteForTransaction(
        txId,
        dataAvailabilityContract.address,
        0,
        calldata,
        signatureToStruct(signature, validator.toPublicKey())
      );

      await tx.wait();
    }

    expect(await dataAvailabilityContract.quorum()).to.equal(5);
  });
});
