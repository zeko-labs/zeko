import plonkWasm from '../../../web_bindings/plonk_wasm.js';
import { workerSpec } from './worker-spec.js';
import { getEfficientNumWorkers } from './num-workers.js';
import {
  srcFromFunctionModule,
  inlineWorker,
  waitForMessage,
} from './worker-helpers.js';
import snarkyJsWebSrc from 'string:../../../web_bindings/snarky_js_web.bc.js';

export { initSnarkyJS, withThreadPool };

let wasm = plonkWasm();
let init = wasm.default;
/**
 * @type {Worker}
 */
let worker;

async function initSnarkyJS() {
  let { memory } = await init();
  let module = init.__wbindgen_wasm_module;
  let numWorkers = await getEfficientNumWorkers();

  worker = inlineWorker(srcFromFunctionModule(mainWorker));
  await workerCall(worker, 'start', { memory, module, numWorkers });
  globalThis.plonk_wasm = overrideBindings(wasm, worker);

  // we have two approaches to run the .bc.js code after its dependencies are ready, without fetching an additional script:

  // 1. wrap it inside a function and just include that function in the bundle
  // this could be nice and simple, but breaks because the .bc.js code uses `(function(){return this}())` to access `window`
  // (probably as a cross-platform way to get the global object before globalThis existed)
  // that obsolete hack doesn't work here because inside an ES module, this === undefined instead of this === window
  // it seems to work when we patch the source code (replace IIFEs with `window`)

  // 2. include the code as string and eval it:
  // (this works because it breaks out of strict mode)
  new Function(snarkyJsWebSrc)();
}

async function withThreadPool(run) {
  if (worker === undefined) throw Error('need to initialize worker first');
  await workerCall(worker, 'initThreadPool');
  let result;
  try {
    result = await run();
  } finally {
    await workerCall(worker, 'exitThreadPool');
  }
  return result;
}

async function mainWorker() {
  const wasm = plonkWasm();
  let init = wasm.default;

  let spec = workerSpec(wasm);

  let isInitialized = false;
  let data = await waitForMessage(self, 'start');
  let { module, memory, numWorkers } = data.message;

  onMessage(self, 'run', ({ name, args, u32_ptr }) => {
    let functionSpec = spec[name];
    let specArgs = functionSpec.args;
    let resArgs = args;
    for (let i = 0, l = specArgs.length; i < l; i++) {
      let specArg = specArgs[i];
      if (specArg && specArg.__wrap) {
        // Class info got lost on transfer, rebuild it.
        resArgs[i] = specArg.__wrap(args[i].ptr);
      } else {
        resArgs[i] = args[i];
      }
    }
    let res = wasm[name].apply(wasm, resArgs);
    if (functionSpec.res && functionSpec.res.__wrap) {
      res = res.ptr;
    } else if (functionSpec.res && functionSpec.res.there) {
      res = functionSpec.res.there(res);
    }
    /* Here be undefined behavior dragons. */
    wasm.set_u32_ptr(u32_ptr, res);
    /*postMessage(res);*/
  });

  workerExport(self, {
    async initThreadPool() {
      if (!isInitialized) {
        isInitialized = true;
        await wasm.initThreadPool(numWorkers);
      }
    },
    async exitThreadPool() {
      if (isInitialized) {
        isInitialized = false;
        await wasm.exitThreadPool(numWorkers);
      }
    },
  });

  await init(module, memory);
  postMessage({ type: data.id });
}
mainWorker.deps = [
  plonkWasm,
  workerSpec,
  workerExport,
  onMessage,
  waitForMessage,
];

function overrideBindings(wasm, worker) {
  let spec = workerSpec(wasm);
  let plonk_wasm_ = { ...wasm };
  for (let key in spec) {
    plonk_wasm_[key] = (...args) => {
      let u32_ptr = wasm.create_zero_u32_ptr();
      worker.postMessage({
        type: 'run',
        message: { name: key, args, u32_ptr },
      });
      /* Here be undefined behavior dragons. */
      let res = wasm.wait_until_non_zero(u32_ptr);
      wasm.free_u32_ptr(u32_ptr);
      let res_spec = spec[key].res;
      if (res_spec && res_spec.__wrap) {
        return spec[key].res.__wrap(res);
      } else if (res_spec && res_spec.back) {
        return res_spec.back(res);
      } else {
        return res;
      }
    };
  }
  return plonk_wasm_;
}

// helpers for main thread <-> worker communication

function onMessage(worker, type, onMsg) {
  worker.addEventListener('message', function ({ data }) {
    if (data?.type !== type) return;
    onMsg(data.message);
  });
}

function workerExport(worker, exportObject) {
  for (let key in exportObject) {
    worker.addEventListener('message', async function ({ data }) {
      if (data?.type !== key) return;
      let result = await exportObject[key](data.message);
      postMessage({ type: data.id, result });
    });
  }
}

async function workerCall(worker, type, message) {
  let id = Math.random();
  let promise = waitForMessage(worker, id);
  worker.postMessage({ type, id, message });
  return (await promise).result;
}