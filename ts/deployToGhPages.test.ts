import { describe, it } from "https://deno.land/std@0.206.0/testing/bdd.ts";
import {
  assertEquals,
  assertMatch,
  assertNotEquals,
} from "https://deno.land/std@0.206.0/assert/mod.ts";
import * as garn from "./mod.ts";
import { assertSuccess, runCommand, runExecutable } from "./testUtils.ts";
import outdent from "https://deno.land/x/outdent@v0.8.0/mod.ts";

const project = garn
  .mkProject(
    {
      description: "",
      defaultEnvironment: garn.emptyEnvironment,
    },
    {
      buildProject: garn.build`mkdir -p $out/assets ; echo built > $out/assets/artifact ; echo hidden > $out/.hidden`,
    },
  )
  .add(garn.deployToGhPages((self) => self.buildProject));

const mkGitRepo = () => {
  const path = Deno.makeTempDirSync();
  const run = (...args: Array<string>) => {
    const output = assertSuccess(
      runCommand(new Deno.Command("git", { args, cwd: path })),
    );
    return output.stdout;
  };
  run("init", "--initial-branch=main");
  run("commit", "--allow-empty", "-m", "first commit");
  return { run, path };
};

const ansiRegexp = "\\x1b\\[[0-9;]+m";

describe("deployToGhPages", () => {
  it("allows to create a commit on a new 'gh-pages' branch", () => {
    const gitRepo = mkGitRepo();
    const output = assertSuccess(
      runExecutable(project.deployToGhPages, {
        cwd: gitRepo.path,
        inSameDir: false,
      }),
    );
    assertMatch(
      output.stderr,
      regexp`
        ^
        warning: creating lock file [^\\n]+\\n
        (
        (this|these \\d+) derivations? will be built:\\n
        ([^\\n]+\\n)+
        (building[^\\n]+\\n)+
        )?
        Created commit to "gh-pages" branch, but it has not been pushed yet\\n
        Run ${ansiRegexp}git push <remote> gh-pages:gh-pages${ansiRegexp} to deploy\\n
        $
      `,
    );
    gitRepo.run("checkout", "gh-pages");
    assertEquals(
      Array.from(Deno.readDirSync(gitRepo.path))
        .map((x) => x.name)
        .sort(),
      [".git", ".hidden", "assets"],
    );
    assertEquals(
      Deno.readTextFileSync(`${gitRepo.path}/assets/artifact`),
      "built\n",
    );
    assertMatch(
      gitRepo.run("log", "-n1", "--pretty=format:%s"),
      /^Deploy [0-9a-f]{7} to gh-pages$/,
    );
    assertEquals(gitRepo.run("status", "--short"), "");
  });

  it("allows to create a commit on an existing 'gh-pages' branch", () => {
    const gitRepo = mkGitRepo();
    gitRepo.run("checkout", "--orphan", "gh-pages");
    Deno.writeTextFileSync(`${gitRepo.path}/foo`, "some existing content");
    Deno.writeTextFileSync(`${gitRepo.path}/.hidden`, "some hidden content");
    gitRepo.run("add", "foo", ".hidden");
    gitRepo.run("commit", "-m", "Add some files");
    gitRepo.run("checkout", "main");
    assertSuccess(
      runExecutable(project.deployToGhPages, { cwd: gitRepo.path, inSameDir: false }),
    );
    gitRepo.run("checkout", "gh-pages");
    assertEquals(
      Array.from(Deno.readDirSync(gitRepo.path))
        .map((x) => x.name)
        .sort(),
      [".git", ".hidden", "assets"],
    );
    assertEquals(
      Deno.readTextFileSync(`${gitRepo.path}/assets/artifact`),
      "built\n",
    );
    assertMatch(
      gitRepo.run("log", "--pretty=format:%s"),
      /^Deploy [0-9a-f]{7} to gh-pages\nAdd some files$/,
    );
    assertEquals(gitRepo.run("status", "--short"), "");
  });

  it("does not commit files from the source branch to the 'gh-pages' branch", () => {
    const gitRepo = mkGitRepo();
    Deno.writeTextFileSync(`${gitRepo.path}/foo`, "some existing content");
    Deno.writeTextFileSync(`${gitRepo.path}/.hidden`, "some hidden content");
    gitRepo.run("add", "foo", ".hidden");
    gitRepo.run("commit", "-m", "Add some files");
    assertSuccess(
      runExecutable(project.deployToGhPages, { cwd: gitRepo.path, inSameDir: false }),
    );
    gitRepo.run("checkout", "gh-pages");
    assertEquals(
      Array.from(Deno.readDirSync(gitRepo.path))
        .map((x) => x.name)
        .sort(),
      [".git", ".hidden", "assets"],
    );
    assertMatch(
      gitRepo.run("log", "--pretty=format:%s"),
      /^Deploy [0-9a-f]{7} to gh-pages$/,
    );
  });

  it("does not affect working-tree changes", () => {
    const gitRepo = mkGitRepo();
    Deno.writeTextFileSync(`${gitRepo.path}/foo`, "some existing content");
    gitRepo.run("add", "foo");
    gitRepo.run("commit", "-m", "Add foo");
    Deno.writeTextFileSync(`${gitRepo.path}/foo`, "foo has been modified");
    Deno.writeTextFileSync(
      `${gitRepo.path}/untracked`,
      "some untracked content",
    );
    assertSuccess(
      runExecutable(project.deployToGhPages, { cwd: gitRepo.path, inSameDir: false }),
    );
    assertEquals(
      Array.from(Deno.readDirSync(gitRepo.path))
        .map((x) => x.name)
        .sort(),
      [".git", "foo", "untracked"],
    );
  });

  it("throws an error if the gh-pages branch is already checked out", () => {
    const gitRepo = mkGitRepo();
    gitRepo.run("checkout", "-b", "gh-pages");
    const output = runExecutable(project.deployToGhPages, {
      cwd: gitRepo.path,
      inSameDir: false,
    });
    assertNotEquals(output.exitCode, 0);
    assertMatch(
      output.stderr,
      regexp`
        ^
        warning: creating lock file [^\\n]+\\n
        ${ansiRegexp}error:
        ${ansiRegexp} deployToGhPages cannot run if gh-pages is currently checked out. Please change branches first.${ansiRegexp}\\n
        $
      `,
    );
  });
});

function regexp(f: TemplateStringsArray, ...templates: Array<string>) {
  return new RegExp(outdent(f, ...templates).replaceAll("\n", ""));
}
