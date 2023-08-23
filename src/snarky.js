import { proxyClasses } from './bindings/js/proxy.js';
import snarkySpec from './bindings/js/snarky-class-spec.js';
import { getSnarky, withThreadPool } from './bindings/js/wrapper.js';

export { Async_js, Ledger, Pickles, Rollup, Snarky, Test, withThreadPool };
let isReadyBoolean = true;
let isItReady = () => isReadyBoolean;

let { Snarky, Ledger, Pickles, Test, Rollup, Async_js } = proxyClasses(
  getSnarky,
  isItReady,
  snarkySpec
);
