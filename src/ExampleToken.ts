import { PublicKey, SmartContract, UInt64, method } from 'snarkyjs';

export class ExampleToken extends SmartContract {
  @method mintTokens(receiverAddress: PublicKey, amount: UInt64) {
    this.token.mint({
      address: receiverAddress,
      amount,
    });
  }

  @method sendTokens(
    senderAddress: PublicKey,
    receiverAddress: PublicKey,
    amount: UInt64
  ) {
    this.token.send({
      to: receiverAddress,
      from: senderAddress,
      amount,
    });
  }
}
