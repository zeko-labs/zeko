import { Field, Poseidon } from 'snarkyjs';

const prefixToField = (prefix: string) => {
  if (prefix.length * 8 >= 255) throw Error('prefix too long');
  let bits = [...prefix]
    .map((char) => {
      // convert char to 8 bits
      let bits = [];
      for (let j = 0, c = char.charCodeAt(0); j < 8; j++, c >>= 1) {
        bits.push(!!(c & 1));
      }
      return bits;
    })
    .flat();
  return Field.fromBits(bits);
};

export const salt = (prefix: string) => {
  return Poseidon.update(Poseidon.initialState(), [prefixToField(prefix)]);
};

export const hashWithPrefix = (prefix: string, input: Field[]) => {
  return Poseidon.update(salt(prefix) as [Field, Field, Field], input)[0];
};

export const updateActionsState = (state: Field, actionHash: Field) => {
  return hashWithPrefix('MinaZkappSeqEvents**', [state, actionHash]);
};

export const hashAction = (action: Field[]) => {
  const emptyActionsHash = salt('MinaZkappActionsEmpty')[0];
  const eventHash = hashWithPrefix('MinaZkappEvent******', action);
  return hashWithPrefix('MinaZkappSeqEvents**', [emptyActionsHash, eventHash]);
};
