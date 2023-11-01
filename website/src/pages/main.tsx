import React from "react";
import { DiscordLink, GithubLink } from "../components/HeaderLinks";
import { Tooltip } from "../components/Hovernote";
import { Garnix, Typography } from "../components/Typography";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";
import { Code } from "../components/Code";
import { LandingPageScenario } from "../components/LandingPageScenario";
import { Hero } from "../components/Hero";

const Garn = () => <span className="garnix-name">garn</span>;

export const Main: React.FC = () => {
  return (
    <>
      <header className="nav">
        <Typography variant="h1">
          <Link to="/">garn</Link>
        </Typography>
        <nav>
          <NavLink
            className={({ isActive }) => (isActive ? "black" : "gray")}
            to="/"
          >
            home
          </NavLink>
          {docMenuItems.map(({ name, url }) => (
            <React.Fragment key={name}>
              {url.match(/^(?:https?:)?\/\//) ? (
                <a
                  href={url}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="gray"
                >
                  {name}
                </a>
              ) : (
                <NavLink
                  to={url}
                  className={({ isActive }) => (isActive ? "black" : "gray")}
                >
                  {name}
                </NavLink>
              )}
            </React.Fragment>
          ))}
        </nav>
        <div className="links">
          <DiscordLink
            href="https://discord.gg/XtDrPsqpVx"
            title="Join the Discord Channel"
          />
          <GithubLink
            href="https://github.com/garnix-io/garn"
            title="View on GitHub"
          />
        </div>
      </header>

      <article>
        <div className="main-content">
          <Outlet />
        </div>
      </article>
      <footer>
        <p>
          Built by{" "}
          <a href="https://garnix.io" className="garnix-name">
            garnix
          </a>
          .
        </p>
      </footer>
    </>
  );
};

export const Info: React.FC = () => {
  return (
    <>
      <Hero />
      <LandingPageScenario
        title="Have you ever worked on a project and realized you had the wrong node version?"
        description={<>
          Or golang? Or prettier? Or some other tool? <Garn /> manages all of
          your project's dependencies in encapsulated environments. Compilers,
          code-generators, formatters, test-runners, linters and more.
        </>}
        examples={<>
          <Code
            header="garn.ts"
            lineNumbers
            code={`import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";

export const frontend = garn.javascript.mkNpmProject({
  description: "my frontend project",
  src: ".",
  nodeVersion: "18",
});`}
          />
          <Code
            header="terminal"
            code={`$ node
node: command not found
$ garn enter frontend
[garn] Entering frontend shell. Type 'exit' to exit.
$ node --version
v18.17.1`}
          />
        </>}
      />
      <LandingPageScenario
        title="Have you ever pushed to the same branch on GitHub over and over again, just to get CI working?"
        description={<>
          <Garn /> allows to declare reproducible checks that you can run
          locally in completely deterministic environments. And if you enable{" "}
          <a href="https://garnix.io">
            <Garnix />
          </a>{" "}
          on your GitHub repo, you get the same exact checks on CI.
        </>}
        examples={<>
          <Code
            header="garn.ts"
            lineNumbers
            code={`import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";
import * as nixpkgs from "https://garn.io/ts/v0.0.13/nixpkgs.ts";

export const backend = garn.haskell
  .mkHaskellProject({
    description: "my haskell backend",
    executable: "server",
    compiler: "ghc94",
    src: ".",
  })
  .withDevTools([nixpkgs.hlint])
  .addCheck("hlint")\`hlint *.hs\`; `}
          />
          <Code
            header="terminal"
            code={`$ garn check
[...]
check> No hints`}
          />
        </>}
      />
      <LandingPageScenario
        title="Have you ever written a bash script that worked totally fine on your machine, but crashed on someone else's?"
        description={<>
          Well, it was probably their fault for not having the right tools
          installed... <Garn /> allows you to add deterministic scripts to your
          projects that run the same everywhere. Use them to run dev servers,
          bundle javascript, format source code, run code generators and more.
        </>}
        examples={<>
          <Code
            header="garn.ts"
            lineNumbers
            code={`export const backend = garn.go.mkGoProject({
  description: "Go backend",
  src: ".",
  goVersion: "1.20",
})
  .withDevTools([pkgs.protobuf, pkgs.protoc_gen_go])
  .addExecutable("codegen")\`protoc --go_out=out protobufs/*.proto\` `}
          />
          <Code
            header="terminal"
            code={`$ protoc protobufs/*.proto
protoc: command not found

$ garn run backend.codegen `}
          />
        </>}
      />
      <section className="faq">
        <h1>Frequently Asked Questions</h1>

        <details>
          <summary>How does garn differ from docker, vagrant, etc.?</summary>
          <p>
            <Garn /> uses nix instead of containers or VMs to provide
            dependencies. These dependencies will all be installed into the nix
            store (in <code>/nix/store</code>) and will be provided from there
            in the environments that you create with <Garn />.
          </p>
          <p>
            That means that <Garn /> scripts and checks and everything else you
            do when developing with <Garn /> runs natively on your machine. So
            there is no overhead that you would get when using VMs or containers
            on non-linux hosts.
          </p>
          <p>
            That also means that -- when entering an environment with garn enter
            -- you can use your personal editor or other globally installed
            tools with no additional work.
          </p>
        </details>

        <details>
          <summary>
            How does garn differ from npm, cabal and other language specific
            tools?
          </summary>
          <p>
            <Garn /> is language agnostic. That means you can use it in projects
            that mix multiple languages and have the same experience and
            workflows for all sub-projects. It also means that programmers that
            are unfamiliar with the toolchain of a project can use <Garn /> to
            get started quickly.
          </p>
        </details>

        <details>
          <summary>
            How can I configure LSP to get error messages and auto-completion
            for garn.ts in my editor?
          </summary>
          <p>
            <code>garn.ts</code> files are powered by{" "}
            <a href="https://deno.com/">deno</a>. A lot of the convenience and
            power for editing your <code>garn.ts</code> files comes from having
            a working Deno LSP. There are two ways of setting up LSP for editing{" "}
            <code>garn.ts</code> files: using <Garn /> itself to provide a
            properly-configured editor with <code>editGarnConfig</code>, or
            using <Garn /> to provide <code>deno</code> and setting up your
            editor configuration yourself.
          </p>
          <h3>
            <code>editGarnConfig</code>
          </h3>
          <p>
            <Garn /> offers an <code>Executable</code> called{" "}
            <code>editGarnConfig</code>. It will spin up a{" "}
            <a href="https://vscodium.com/">vscodium</a> editor that is
            pre-configured for editing <code>garn.ts</code> files. It won't use
            or modify your local vscodium settings, if you have any. You can can
            add it to your <code>garn.ts</code> file like this:
          </p>
          <Code
            header="garn.ts"
            lineNumbers
            code={`import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";

export const edit = garn.editGarnConfig; `}
          />
          <p>
            And then run it with <code>garn run edit</code>.
          </p>
          <p>
            This is a very easy way to get up and running with editing{" "}
            <code>garn.ts</code> files. The disadvantage is that the editor
            isn't your normal configured editor. So you might consider
            installing the Deno LSP and configuring your editor:
          </p>
          <h3>Installing the Deno LSP and configuring your editor</h3>
          <p>
            Alternatively you an install <code>deno</code> (including the Deno
            LSP) with <Garn /> itself:
          </p>
          <Code
            header="garn.ts"
            lineNumbers
            code={`import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";

export const deno = garn.mkProject({
  description: "garn configuration environment",
  defaultEnvironment: garn.emptyEnvironment.withDevTools([pkgs.deno]),
}, {});`}
          />
          <p>
            <code>garn enter deno</code> will then drop you in a shell where{" "}
            <code>deno</code> is available.
          </p>
          <p>
            For configuring your editor to use Deno's LSP refer to{" "}
            <a href="https://docs.deno.com/runtime/manual/getting_started/setup_your_environment">
              Deno's environment setup documentation
            </a>
            .
          </p>
        </details>

        <details>
          <summary>
            I'm getting no such file or directory errors for files that exist.
            What's going on?
          </summary>
          <p>
            <Garn /> uses nix under the hood which requires files in a git
            repository to be tracked by git in order to see the files.
          </p>
          <p>
            To resolve this make sure to either add untracked files to git (e.g.
            with <code>git add $YOUR_FILES</code>), or mark them as intended
            additions with <code>git add --intent-to-add $YOUR_FILES</code>
          </p>
          <p>
            Likely this requirement will be removed from future <Garn />{" "}
            versions.
          </p>
        </details>
      </section>
    </>
  );
};
