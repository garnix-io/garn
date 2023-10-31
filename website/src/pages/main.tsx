import React from "react";
import { DiscordLink, GithubLink } from "../components/HeaderLinks";
import { Tooltip } from "../components/Hovernote";
import { Typography } from "../components/Typography";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";
import { Code } from "../components/Code";
import { Image } from "../components/Image";

const Keyword: React.FC = (props) => (
  <span className="keyword">{props.children}</span>
);
const String: React.FC = (props) => (
  <span className="string">{props.children}</span>
);
const Export: React.FC = (props) => (
  <span className="export">{props.children}</span>
);

const Garn = () => <span className="garnix-name">garn</span>;

const garnTs = (
  <>
    <Keyword>import</Keyword> * as garn from{" "}
    <String>"https://garn.io/ts/v0.0.13/mod.ts"</String>;<br />
    <Keyword>import</Keyword> * as{" "}
    <Tooltip item="pkgs">
      {`This is a gigantic collection
of packages, nixpkgs. If you
need a tool or dependency,
it's probably here`}
    </Tooltip>{" "}
    from <String>"https://garn.io/ts/v0.0.13/nixpkgs.ts"</String>;<br />
    <br />
    <Export>export</Export> <Keyword>const</Keyword>{" "}
    <Tooltip item="backend">
      {`A Project is a collection of
packages, environments, and
executables.
`}
    </Tooltip>{" "}
    = garn.go.mkGoProject(&#123; <br />
    {"  "}description: <String>"A go server"</String>,<br />
    {"  "}src: <String>"."</String>,<br />
    {"  "}goVersion: <String>"1.20"</String>,<br />
    &#125;)
    <br />
    {"  "}.
    <Tooltip item="withDevTools">
      {`withDevTools is all you need
to add a dev tool to your shell.
`}
    </Tooltip>
    ([pkgs.protobuf, pkgs.protoc_gen_go])
    <br />
    {"  "}.
    <Tooltip item="addCheck">
      addCheck adds a *pure* check that you can run locally or on CI.
    </Tooltip>
    (<String>"ensure-formatted"</String>)<String>`go fmt ./...`</String>
    <br />
    {"  "}.<Tooltip item="addExecutable">addExecutable</Tooltip>(
    <String>"start"</String>)<String>`go run main.go`</String>;
  </>
);

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
            title="View on Github"
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

const terminalExamples = [
  "check backend.ensure-formatted",
  "run backend.start",
  "enter backend",
  "build backend",
];

export const Info: React.FC = () => {
  const [termState, setTermState] = React.useState({
    deleting: false,
    idx: 0,
    text: "",
  });
  const currentExample = terminalExamples[termState.idx];

  React.useEffect(() => {
    let timeout: NodeJS.Timeout;
    const type = (delay: number, newState: Partial<typeof termState>) => {
      timeout = setTimeout(() => {
        setTermState({
          ...termState,
          ...newState,
        });
      }, delay);
    };
    if (termState.deleting) {
      if (termState.text.length > 0) {
        type(5 + Math.random() * 15, {
          text: currentExample.slice(0, termState.text.length - 1),
        });
      } else {
        type(50, {
          deleting: false,
          idx: (termState.idx + 1) % terminalExamples.length,
        });
      }
    } else {
      if (termState.text.length < currentExample.length) {
        type(20 + Math.random() * 45, {
          text: currentExample.slice(0, termState.text.length + 1),
        });
      } else {
        type(2500, { deleting: true });
      }
    }
    return () => clearTimeout(timeout);
  });
  return (
    <>
      <div className="hero">
        <div>
          <h1>garn</h1>
          <h2>A new way of configuring software projects</h2>
          <div className="ctas">
            <Link to="/docs/tutorial">Read More</Link>
            <Link to="/docs/getting_started">Get Started</Link>
          </div>
        </div>
        <div className="">
          <Code header="garn.ts" lineNumbers code={garnTs} />
          <Code
            header="terminal"
            code={
              <>
                $ garn {termState.text}
                <div className="cursor" />
              </>
            }
          />
        </div>
      </div>
      <section className="scenario">
        <h1>
          Did you ever work on a project and realized you had the wrong node
          version?
        </h1>
        <p>
          Or golang? Or prettier? Or some other tool? <Garn /> manages all of
          your project's dependencies in encapsulated environments. Compilers,
          code-generators, formatters, test-runners, linters and more.{" "}
          <a href="./docs/tutorial">Read more...</a>
          <div>
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
          </div>
        </p>
      </section>
      <section className="scenario">
        <h1>
          Did you ever push to the same branch on Github over and over again,
          just to get CI working like it was locally?
        </h1>
        <p>
          <Garn /> allows to declare reproducible checks that you can run
          locally and on CI on Github.{" "}
          <a href="./docs/tutorial">Read more...</a>
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
          <Image src="/images/github_hlint_ci_check.png" header="github ui" />
        </p>
      </section>
      <section className="scenario">
        <h1>
          Did you ever write a bash script for your project that worked totally
          fine on your own machine, but crashed on someone else's?
        </h1>
        <p>
          Well, it was probably their fault for not having the right tools
          installed... <Garn /> allows you to write deterministic scripts that
          run the same everywhere. For running dev servers, bundling javascript,
          formatting source code, running code generators and more.{" "}
          <a href="./docs/tutorial">Read more...</a>
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
        </p>
      </section>
      <section className="faq">
        <h1>Frequently Asked Questions</h1>

        <details>
          <summary>How does garn differ from docker, vagrant, etc?</summary>
          <p>
            <Garn /> uses nix instead of containers or VMs to provide
            dependencies. nix has a few advantages that makes it easier to work
            with:
          </p>
          <p>
            For one, nix runs natively on your machine so there is no overhead
            of of a VM that you get when running containers on non-linux hosts.
          </p>
          <p>
            Additionally, nix allows you to combine the declarative environment
            with your developer environment so entering a shell with all the
            tools for a project does not remove your ability to use your
            globally installed tools as well.
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
            <code>garn.ts</code>
            files: using <Garn /> itself to provide a properly-configured editor
            with
            <code>editGarnConfig</code>, or setting up your editor yourself.
          </p>
          <h3>
            <code>editGarnConfig</code>
          </h3>
          <p>
            <Garn /> offers an <code>Executable</code> called{" "}
            <code>editGarnConfig</code>. It will spin up a vscodium editor that
            is pre-configured for editing <code>garn.ts</code> files. It won't
            use or modify your local vscodium settings, if you have any. You can
            can add it to your <code>garn.ts</code> file like this:
          </p>
          <Code
            header="garn.ts"
            lineNumbers
            code="export const edit = garn.editGarnConfig;"
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
            code={`export const deno = garn.mkProject({
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
            repository be tracked by git in order to see the files.
          </p>
          <p>
            To resolve this make sure to either add untracked files to the git
            index, or at least mark them as intended additions with{" "}
            <code>git add --intent-to-add</code>
          </p>
          <p>
            Likely this requirement will be removed from future <Garn />{" "}
            versions
          </p>
        </details>
      </section>
    </>
  );
};
