import {
  AccountUpdate,
  Provable,
  PublicKey,
  SmartContract,
  UInt64,
  method,
} from 'snarkyjs';

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

    return this.self;
  }

  @method approveTransfer(from: AccountUpdate, to: AccountUpdate) {
    Provable.log(from);
    Provable.log(to);

    from.body.balanceChange.sgn.isPositive().assertFalse();
    to.body.balanceChange.sgn.isPositive().assertTrue();

    from.body.balanceChange.magnitude.assertEquals(
      to.body.balanceChange.magnitude
    );

    from.body.tokenId.assertEquals(this.token.id);
    to.body.tokenId.assertEquals(this.token.id);

    from.approve(from, AccountUpdate.Layout.NoChildren);
    to.approve(from, AccountUpdate.Layout.NoChildren);
  }
}
