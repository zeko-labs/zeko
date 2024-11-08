NB: unimplemented, incomplete

We want to support staking your MINA held on Zeko.
Who you choose to delegate to on Zeko is meaningless,
since the zkapp that holds the funds can only delegate to one.
Since there are multiple such accounts, you could theoretically
delegate to multiple, but we won't support that here.
If you are so inclined, you could wrap MINA in a different
way on Zeko with your own contract that does the staking differently.

Currently, on MINA, staking rewards are not distributed
in a trustless manner. It is done merely as a payment.

We thus need to track payments made into the account
that aren't deposits.
We create a token account that tracks the "real" balance,
on deposit we add to its balance, and on withdrawal we withdraw
from its balance, which is denominated in our own token, i.e.
we mint and burn it.

In the epoch after, we check the staking ledger hash,
and emit an action that contains how much was sent to
the account, along with the Zeko ledger hash at the time,
such that at another point withdrawals can be done using this
information.
