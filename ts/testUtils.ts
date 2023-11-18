import { assertEquals } from "https://deno.land/std@0.206.0/assert/mod.ts";
import * as garn from "./mod.ts";
import * as nix from "./nix.ts";
import { nixAttrSet } from "./nix.ts";

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

export const assertSuccess = (output: Output): Output => {
  try {
    assertEquals(output.exitCode, 0);
    return output;
  } catch (e) {
    printOutput(output);
    throw e;
  }
};

export const assertStdout = (output: Output, expected: string) => {
  try {
    assertEquals(output.stdout, expected);
  } catch (e) {
    printOutput(output);
    throw e;
  }
};

export const assertStderr = (output: Output, expected: string) => {
  try {
    assertEquals(output.stderr, expected);
  } catch (e) {
    printOutput(output);
    throw e;
  }
};

export const runExecutable = (
  executable: garn.Executable,
  options: { cwd?: string } = {},
): Output => {
  const tempDir = options.cwd ? options.cwd : Deno.makeTempDirSync({ prefix: "garn-test" });
  const nixpkgsInput = nix.nixFlakeDep("nixpkgs-repo", {
    url: "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566",
  });
  const flakeFile = nix.renderFlakeFile(
    nixAttrSet({
      apps: nixAttrSet({
        "x86_64-linux": nixAttrSet({
          default: nixAttrSet({
            type: nix.nixStrLit`app`,
            program: nix.nixRaw`
                  let pkgs = import ${nixpkgsInput} {
                        config.allowUnfree = true;
                        system = "x86_64-linux";
                      };
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
