export {
  Group,
  Scalar,
  AsFieldElements,
  Ledger,
  isReady,
  shutdown,
} from './snarky';
export { Field, Bool } from './lib/core';
export type { VerificationKey, Keypair } from './snarky';
export * from './snarky/addons';
export { Poseidon } from './lib/hash';
export * from './lib/signature';
export {
  Circuit,
  CircuitValue,
  prop,
  arrayProp,
  matrixProp,
  public_,
  circuitMain,
  circuitValue,
} from './lib/circuit_value';
export { UInt32, UInt64, Int64, Sign } from './lib/int';
export { Types } from './snarky/types';

export * as Mina from './lib/mina';
export {
  SmartContract,
  method,
  deploy,
  DeployArgs,
  signFeePayer,
  declareMethods,
} from './lib/zkapp';
export { state, State, declareState } from './lib/state';
export { Proof, SelfProof, ZkProgram, verify } from './lib/proof_system';
export {
  Token,
  Party,
  Permissions,
  ZkappPublicInput,
  partiesToJson,
} from './lib/party';
export {
  fetchAccount,
  fetchLastBlock,
  addCachedAccount,
  setGraphqlEndpoint,
  sendZkapp,
} from './lib/fetch';
export * as Encryption from './lib/encryption';
export * as Encoding from './lib/encoding';
export { Character, CircuitString } from './lib/string';

// experimental APIs
import { Reducer } from './lib/zkapp';
import { createChildParty } from './lib/party';
import {
  memoizeWitness,
  AsFieldsAndAux as AsFieldsAndAux_,
} from './lib/circuit_value';
import { jsLayout, asFieldsAndAux } from './snarky/types';
import { packToFields } from './lib/hash';
import { MerkleTree, MerkleWitness } from './lib/merkle_tree';
export { Experimental };

const Experimental_ = {
  Reducer,
  createChildParty,
  memoizeWitness,
  // TODO: for testing, maybe remove later
  jsLayout,
  asFieldsAndAux,
  packToFields,
  MerkleTree,
  MerkleWitness,
};

/**
 * This module exposes APIs that are unstable, in the sense that the API surface is expected to change.
 * (Not unstable in the sense that they are less functional or tested than other parts.)
 */
namespace Experimental {
  export let Reducer = Experimental_.Reducer;
  export let createChildParty = Experimental_.createChildParty;
  export let memoizeWitness = Experimental_.memoizeWitness;
  export let jsLayout = Experimental_.jsLayout;
  export let asFieldsAndAux = Experimental_.asFieldsAndAux;
  export let packToFields = Experimental_.packToFields;
  export let MerkleTree = Experimental_.MerkleTree;
  export let MerkleWitness = Experimental_.MerkleWitness;
  export type AsFieldsAndAux<T, TJson> = AsFieldsAndAux_<T, TJson>;
}