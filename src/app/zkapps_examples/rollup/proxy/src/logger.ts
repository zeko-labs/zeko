import { isMainThread, threadId } from "worker_threads";

const identifier = () => (isMainThread ? "Main:" : `Worker ${threadId}:`);

export default {
  info: (...args: any[]) => console.log(identifier(), ...args),
  error: (...args: any[]) => console.error(identifier(), ...args),
  debug: (...args: any[]) => console.debug(identifier(), ...args),
};
