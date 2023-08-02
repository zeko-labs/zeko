import { z } from 'zod';

const schema = z.object({
  PROOFS_ENABLED: z.preprocess(
    (val) => val === 'true',
    z.boolean().default(false)
  ),
});

export default schema.parse(process.env);
