import { Field, Bool } from '../lib/core.js';
import { UInt32, UInt64, Sign } from '../lib/int.js';
import { PublicKey } from '../lib/signature.js';
import { derivedLeafTypes } from './derived-leaves.js';
import { createEvents } from '../lib/events.js';
import {
  Poseidon,
  Hash,
  packToFields,
  emptyHashWithPrefix,
} from '../lib/hash.js';
import { provable } from '../lib/circuit_value.js';

export { PublicKey, Field, Bool, AuthRequired, UInt64, UInt32, Sign, TokenId };

export {
  Events,
  SequenceEvents,
  ZkappUri,
  TokenSymbol,
  SequenceState,
  ReceiptChainHash,
};

type AuthRequired = {
  constant: Bool;
  signatureNecessary: Bool;
  signatureSufficient: Bool;
};
type TokenId = Field;
type TokenSymbol = { symbol: string; field: Field };
type ZkappUri = { data: string; hash: Field };

const { TokenId, TokenSymbol, AuthRequired, ZkappUri } = derivedLeafTypes({
  Field,
  Bool,
  Hash,
  packToFields,
});

type Event = Field[];
type Events = {
  hash: Field;
  data: Event[];
};
type SequenceEvents = Events;
const { Events, SequenceEvents } = createEvents({ Field, Poseidon });

type SequenceState = Field;
const SequenceState = {
  ...provable(Field),
  emptyValue: SequenceEvents.emptySequenceState,
};

type ReceiptChainHash = Field;
const ReceiptChainHash = {
  ...provable(Field),
  emptyValue: () => emptyHashWithPrefix('CodaReceiptEmpty'),
};