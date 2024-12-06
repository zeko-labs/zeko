# The manner of upgrading

Upgrading in a centralized manner happens by changing vk with signature on account update.
Upgrading in a decentralized manner is not yet easy enough, but in the future once zkapps can look
up in the account update call forest, it will be done by imitating a signature on the full
commitment of the transaction by way of governance.

The upgrade must be doable in a non-atomic piece-meal manner, since there are simply too many
accounts to include in a single account update.
Consider all the helper accounts for the bridge zkapps.

Consider migrating the zkapp from one address to another, i.e.
we abandon the old account.
This would ensure that other zkapps (all of which we might not know of)
do not misinterpret the new state in the format of the old state.i

This is practical, except that we now leave the old account unusable.
If we delete the old account, we run into the risk that the original owner
of the private key will recreate it with a different state.

This is acceptable in the centralized world, but not in the decentralized one.

Consider another approach: We can have a version field in the zkapp state.
Then however all dependent zkapps need to make sure they match on the version field,
which we can not ensure, but if they don't, it would be their fault and not ours.

---

Consider the helper accounts for the bridge zkapps.
We can not feasibly upgrade every one at the same time, nor can we do it
proactively, since the user should pay the fee themself.

Consider adding a version field to the helper accounts.
All zkapps that deal with the helper accounts would then check the version field
to make sure it's been upgraded.
The user must perform the upgrade by themself then.
