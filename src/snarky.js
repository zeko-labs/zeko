import './bindings/crypto/bindings.js';
import { proxyClasses } from './bindings/js/proxy.js';
import snarkySpec from './bindings/js/snarky-class-spec.js';
import { getSnarky, withThreadPool } from './bindings/js/wrapper.js';

export {
  Async_js,
  Ledger,
  Pickles,
  RollupBindings,
  Snarky,
  Test,
  withThreadPool,
};
let isReadyBoolean = true;
let isItReady = () => isReadyBoolean;

let { Snarky, Ledger, Pickles, Test, RollupBindings, Async_js } = proxyClasses(
  getSnarky,
  isItReady,
  snarkySpec
);
