import { z } from "https://deno.land/x/zod@v3.22.4/mod.ts";
import { Result, parseToml } from "../internal/utils.ts";

const pyprojectTomlSchema = z.object({
  project: z.object({
    name: z.string(),
    version: z.string(),
    description: z.string().optional(),
    scripts: z.record(z.string()).optional(),
    dependencies: z.array(z.string()).optional(),
  }),
});

export const parsePyprojectToml = (
  contents: string,
): Result<(typeof pyprojectTomlSchema)["_output"]> => {
  const result = parseToml(pyprojectTomlSchema, contents);
  return result;
};
