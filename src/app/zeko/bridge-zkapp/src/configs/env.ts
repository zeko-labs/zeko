import { cleanEnv, url } from "envalid";

const env = cleanEnv(process.env, {
  REACT_APP_SEQUENCER_URL: url(),
});

export default env;
