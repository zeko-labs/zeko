import { wasm, withThreadPool } from './node/node-backend.js';
import { default as snarky } from '../compiled/_node_bindings/snarky_js_node.bc.cjs';

export { getSnarky, getWasm, withThreadPool };

let getSnarky = () => snarky;

wasm.console_error_panic_hook_set_once();

function getWasm() {
  return wasm;
}
