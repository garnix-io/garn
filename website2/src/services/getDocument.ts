import { readFile } from "fs/promises";
import { serialize } from "next-mdx-remote/serialize";
import { type MDXRemoteSerializeResult } from "next-mdx-remote";

export const getDocument = async (
  slug: string
): Promise<MDXRemoteSerializeResult> => {
  const raw = await readFile(`documentation/${slug}.mdx`, "utf-8");
  return serialize(raw, {
    parseFrontmatter: true,
  });
};
