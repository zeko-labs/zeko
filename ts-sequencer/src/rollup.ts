import { Ledger, Mina, PublicKey } from 'snarkyjs';

export type Account = {
  publicKey: PublicKey;
  balance: number | string;
};

export type RollupContext = {
  rollup: Rollup;
};

export class Rollup {
  public ledger: Ledger;

  public readonly networkState;
  public readonly networkConstants;

  constructor(genesisAccounts: Account[], accountCreationFee: number | string) {
    this.ledger = Ledger.create(
      genesisAccounts.map(({ publicKey, balance }) => ({
        publicKey,
        balance: balance.toString(),
      }))
    );

    const dummyBlockchain = Mina.LocalBlockchain({
      accountCreationFee: accountCreationFee.toString(),
      proofsEnabled: true,
    });

    this.networkState = dummyBlockchain.getNetworkState();
    this.networkConstants = dummyBlockchain.getNetworkConstants();
  }
}
