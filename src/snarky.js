import { getSnarky, withThreadPool } from './bindings/js/wrapper.js';
import snarkySpec from './bindings/js/snarky-class-spec.js';
import { proxyClasses } from './bindings/js/proxy.js';

export { Snarky, Ledger, RollupCommitments, Pickles, Test, withThreadPool };
let isReadyBoolean = true;
let isItReady = () => isReadyBoolean;

let { Snarky, Ledger, RollupCommitments, Pickles, Test } = proxyClasses(
  getSnarky,
  isItReady,
  snarkySpec
);
