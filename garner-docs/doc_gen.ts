import { denoDoc, path } from "./deps.server.ts";

const TS_FILES_ROOT = path.join(
  path.dirname(path.fromFileUrl(import.meta.url)),
  "../ts",
);

export async function getDocs() {
  const files = await toArr(Deno.readDir(TS_FILES_ROOT));
  const documents = await Promise.all(
    files.map(async (file) =>
      [file.name, await denoDoc(`http://localhost:8777/${file.name}`)] as const
    ),
  );
  return documents.reduce(
    (acc, [fileName, docNodes]) => ({ ...acc, [fileName]: docNodes }),
    {},
  );
}

async function toArr<T>(iter: AsyncIterable<T>): Promise<Array<T>> {
  const arr = [];
  for await (const entry of iter) arr.push(entry);
  return arr;
}
