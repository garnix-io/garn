import { assertEquals } from "https://deno.land/std@0.206.0/assert/mod.ts";
import * as garn from "./mod.ts";
import * as nix from "./nix.ts";
import { nixAttrSet } from "./nix.ts";
import { mkPackage } from "./mod.ts";

type Output = {
  exitCode: number;
  stdout: string;
  stderr: string;
};

export const runCommand = (command: Deno.Command): Output => {
  const output = command.outputSync();
  return {
    exitCode: output.code,
    stdout: new TextDecoder().decode(output.stdout),
    stderr: new TextDecoder().decode(output.stderr),
  };
};

const printOutput = (output: Output) => {
  console.error(`
    exitcode: ${output.exitCode}
    stdout:
    ${output.stdout}
    stderr:
    ${output.stderr}
  `);
};

export const assertSuccess = (output: Output): Output =>
  assertOnOutput(output, () => assertEquals(output.exitCode, 0));

export const assertStdout = (output: Output, expected: string): Output =>
  assertOnOutput(output, () => assertEquals(output.stdout, expected));

export const assertStderr = (output: Output, expected: string): Output =>
  assertOnOutput(output, () => assertEquals(output.stderr, expected));

export const assertOnOutput = (
  output: Output,
  assertion: () => void,
): Output => {
  try {
    assertion();
  } catch (e) {
    printOutput(output);
    throw e;
  }
  return output;
};

const nixpkgsInput = nix.nixFlakeDep("nixpkgs-repo", {
  url: "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566",
});

const pkgs = nix.nixRaw`
  import ${nixpkgsInput} {
    config.allowUnfree = true;
    system = "x86_64-linux";
  }
`;

/*
 * Run an Executable. If `cwd` is set, run the `nix run` command from that
 * directory, which is also where the flake file is generated (unless
 * `inSameDir` is `false`).
 */
export const runExecutable = (
  executable: garn.Executable,
  options: { cwd?: string; inSameDir?: boolean } = {},
): Output => {
  const inSameDir = options.inSameDir == undefined ? true : options.inSameDir;

  const tempDir =
    options.cwd && inSameDir
      ? options.cwd
      : Deno.makeTempDirSync({ prefix: "garn-test" });
  const flakeFile = nix.renderFlakeFile(
    nixAttrSet({
      apps: nixAttrSet({
        "x86_64-linux": nixAttrSet({
          default: nixAttrSet({
            type: nix.nixStrLit`app`,
            program: nix.nixRaw`
              let pkgs = ${pkgs};
              in ${executable.nixExpression}
            `,
          }),
        }),
      }),
    }),
  );
  Deno.writeTextFileSync(`${tempDir}/flake.nix`, flakeFile);
  return runCommand(
    new Deno.Command("nix", {
      args: ["run", tempDir],
      cwd: options.cwd,
    }),
  );
};

export const runCheck = (
  check: garn.Check,
  options: { dir?: string } = {},
): Output => {
  const dir = options.dir ?? Deno.makeTempDirSync({ prefix: "garn-test" });
  const flakeFile = nix.renderFlakeFile(
    nixAttrSet({
      checks: nixAttrSet({
        "x86_64-linux": nixAttrSet({
          default: nix.nixRaw`
            let pkgs = ${pkgs};
            in ${check.nixExpression}
          `,
        }),
      }),
    }),
  );
  Deno.writeTextFileSync(`${dir}/flake.nix`, flakeFile);
  return runCommand(
    new Deno.Command("nix", {
      args: ["flake", "check", "-L", dir],
    }),
  );
};

export const buildPackage = (
  pkg: garn.Package,
  options: { dir?: string } = {},
): string => {
  const dir = options.dir ?? Deno.makeTempDirSync({ prefix: "garn-test" });
  const flakeFile = nix.renderFlakeFile(
    nixAttrSet({
      packages: nixAttrSet({
        "x86_64-linux": nixAttrSet({
          default: nix.nixRaw`
            let pkgs = ${pkgs};
            in ${pkg.nixExpression}
          `,
        }),
      }),
    }),
  );
  Deno.writeTextFileSync(`${dir}/flake.nix`, flakeFile);
  assertSuccess(
    runCommand(
      new Deno.Command("nix", {
        args: ["build", "-L", dir],
        cwd: dir,
      }),
    ),
  );
  return Deno.readLinkSync(`${dir}/result`);
};

export const testPkgs = {
  hello: mkPackage(nix.nixRaw("pkgs.hello"), "hello"),
};
