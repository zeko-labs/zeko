/**
 * This file contains bindings for JSOO written in TS and integrated with our normal code base.
 * It is exposed to JSOO by populating a global variable with an object.
 * It gets imported as the first thing in ../../snarky.js so that the global variable is ready by the time JSOO code gets executed.
 */
import { prefixHashes, prefixHashesLegacy } from '../crypto/constants.js';
import { Bigint256Bindings } from './bindings/bigint256.js';
import { PallasBindings, VestaBindings } from './bindings/curve.js';
import { FpBindings, FqBindings } from './bindings/field.js';
import { FpVectorBindings, FqVectorBindings } from './bindings/vector.js';
import type * as wasmNamespace from '../compiled/node_bindings/plonk_wasm.cjs';
import {
  fieldsFromRustFlat,
  fieldsToRustFlat,
} from './bindings/conversion-base-old.js';
import { proofConversion } from './bindings/conversion-proof.js';
import { conversionCore } from './bindings/conversion-core.js';
import { verifierIndexConversion } from './bindings/conversion-verifier-index.js';
import { oraclesConversion } from './bindings/conversion-oracles.js';

const tsBindings = {
  prefixHashes,
  prefixHashesLegacy,
  ...Bigint256Bindings,
  ...FpBindings,
  ...FqBindings,
  ...VestaBindings,
  ...PallasBindings,
  ...FpVectorBindings,
  ...FqVectorBindings,
  rustConversion: createRustConversion,
};

// this is put in a global variable so that ../kimchi/js/bindings.js finds it
(globalThis as any).__snarkyTsBindings = tsBindings;

type wasm = typeof wasmNamespace;

function createRustConversion(wasm: wasm) {
  let core = conversionCore(wasm);
  let verifierIndex = verifierIndexConversion(wasm, core);
  let oracles = oraclesConversion(wasm);
  let proof = proofConversion(wasm, core);

  return {
    fp: { ...core.fp, ...verifierIndex.fp, ...oracles.fp, ...proof.fp },
    fq: { ...core.fq, ...verifierIndex.fq, ...oracles.fq, ...proof.fq },
    fieldsToRustFlat,
    fieldsFromRustFlat,
    wireToRust: core.wireToRust,
    mapMlArrayToRustVector: core.mapMlArrayToRustVector,
  };
}
