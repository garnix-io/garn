import * as garn from "http://localhost:8777/mod.ts";
import { nixRaw, nixStrLit } from "http://localhost:8777/nix.ts";
import * as pkgs from "http://localhost:8777/nixpkgs.ts";

type MkReq<Method, Params> = {
  id: number;
  method: Method;
  jsonrpc: "2.0";
  params: Params;
};

type InitializeReq = MkReq<"initialize", {
  clientInfo: { version: string; name: string };
  trace: "off";
  capabilities: unknown;
  workspaceFolders: null;
  rootPath: null;
  rootUri: null;
  processId: number;
}>;

type InitializedReq = MkReq<"initialized", {}>;

type Req = InitializeReq | InitializedReq;

type InitializeRes = {
  id: number;
  jsonrpc: "2.0";
  result: {
    capabilities: unknown;
    serverInfo: {
      name: string;
      version: string;
    };
  };
};

function js(fn: () => void): garn.Executable {
  const contents = `(${fn.toString()})()`;
  const file = nixRaw`pkgs.writeText "js.js" ${nixStrLit(contents)}`;
  return garn
    .shell`${pkgs.deno}/bin/deno run --quiet --allow-write --allow-run ${file}`;
}

export const lspProxy = js(async () => {
  async function parseMsgs(
    stream: ReadableStream<Uint8Array>,
    fn: (msg: Req) => Promise<void>,
  ) {
    const decoder = new TextDecoder();
    let cur = "";
    for await (const chunk of stream) {
      const ch = decoder.decode(chunk);
      cur += ch;
      const match = cur.match(/^Content-Length: (\d+)\r\n\r\n/);
      if (!match) throw `NO MATCH - ${cur}`;
      const len = parseInt(match[1], 10);
      const headerLen = `Content-Length: ${len}\r\n\r\n`.length;
      if (cur.length < len + headerLen) continue;
      const data = cur.slice(headerLen, headerLen + len);
      let parsed;
      try {
        parsed = JSON.parse(data);
      } catch {
        throw `JSON PARSE FAIL - ${data}`;
      }
      await fn(parsed);
      cur = cur.slice(headerLen + len);
    }
  }

  async function sendMsg(
    write: (data: Uint8Array) => Promise<number | void> | number | void,
    data: unknown,
  ) {
    const str = JSON.stringify(data) + "\n";
    await write(
      encoder.encode(`Content-Length: ${str.length}\r\n\r\n${str}`),
    );
  }

  const encoder = new TextEncoder();
  const dbgFile = Deno.openSync("dbg", { append: true, create: true });
  const dbg = async (txt: string) =>
    await dbgFile.write(encoder.encode(txt + "\n"));

  const lspCmd = new Deno.Command("deno", {
    args: ["lsp"],
    stdin: "piped",
    stdout: "piped",
  });
  const lsp = lspCmd.spawn();

  const stdin = lsp.stdin.getWriter();
  await stdin.ready;

  const parseIn = parseMsgs(Deno.stdin.readable, async (msg) => {
    await dbg(`IN  => ${JSON.stringify(msg)}\n`);
    await sendMsg((d) => stdin.write(d), msg);
  });

  const parseOut = parseMsgs(lsp.stdout, async (msg) => {
    await dbg(`OUT => ${JSON.stringify(msg)}\n`);
    await sendMsg((d) => Deno.stdout.write(d), msg);
  });

  try {
    await Promise.all([parseIn, parseOut]);
    dbg("EXIT");
  } catch (e) {
    dbg(e.toString());
  }
});
