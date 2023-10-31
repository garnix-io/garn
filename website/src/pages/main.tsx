import React from "react";
import { DropdownMenu } from "../components/Dropdown";
import { DiscordLink, GithubLink } from "../components/HeaderLinks";
import { Tooltip } from "../components/Hovernote";
import { Typography } from "../components/Typography";
import { Asciinema } from "../components/Asciinema";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";
import envDemoCastUrl from "../casts/env-demo.cast";
import buildDemoCastUrl from "../casts/build-demo.cast";
import runDemoCastUrl from "../casts/run-demo.cast";
import lspDemoCastUrl from "../casts/lsp-demo.cast";

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
            <Link to="/">Read More</Link>
            <Link to="/docs/getting_started">Get Started</Link>
          </div>
        </div>
        <div className="">
          <figure>
            <header>garn.ts</header>
            <code className="line-numbers">{garnTs}</code>
          </figure>
          <figure>
            <header>terminal</header>
            <code>
              $ garn {termState.text}
              <div className="cursor" />
            </code>
          </figure>
        </div>
      </div>
      <section>
        <h1>
          Have you ever worked on a project where you had the wrong version of
          node installed? Or golang? Or something else?
        </h1>
        <p>
          <Garn /> manages all of your project dependencies in encapsulated
          environments. <a>Read more</a>
          <div className="more">
            <code>
              <pre>{`import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";

export const frontend = garn.javascript.mkNpmProject({
  description: "my frontend project",
  src: ".",
  nodeVersion: "18",
});`}</pre>
            </code>
            <code>
              <pre>{`$ node
node: command not found
$ garn enter frontend
[garn] Entering frontend shell. Type 'exit' to exit.
$ node --version
v18.17.1`}</pre>
            </code>
          </div>
        </p>
      </section>
      <section>
        <h1>
          Have you ever tried to set up CI and had to push to Github 15 times to
          get it working like it was locally?
        </h1>
        <p>
          <Garn /> allows to declare reproducible checks that you can run
          locally and on CI. <a>Read more</a>
        </p>
      </section>
      <section>
        <h1>
          Have you ever written a bash script for your project that worked
          totally fine on your own machine, but didn't on someone else's?
        </h1>
        <p>
          <Garn /> allows you to write deterministic scripts e.g. bundling, code
          formatting or code generation. <a>Read more</a>
        </p>
        <p className="more">
          With <Garn />, support scripts for your project are run in a
          declarative environment, so if it runs for you, it will run for
          everyone on your team.
          <div className="examples">
            <code>
              <pre>{`export const backend = garn.go.mkGoProject({
  description: "Go backend",
  src: ".",
  goVersion: "1.20",
})
  .withDevTools([pkgs.protobuf, pkgs.protoc_gen_go])
  .addExecutable("codegen")\`protoc --go_out=out protobufs/*.proto\` `}</pre>
            </code>
            <code>
              <pre>{`$ protoc protobufs/*.proto
protoc: command not found

$ garn run backend.codegen `}</pre>
            </code>
          </div>
        </p>
      </section>

      {/*
      <section className="lede">
        <h1>garn, at your service</h1>
        <div className="garn-description">
          <b>garn</b> is a build tool and environment manager. It can partly or
          wholly replace manual README instructions, containerization
          technologies such as docker, package managers such as apt/brew/&c, and
          command runners such as just.
        </div>
        <figure>
          <div className="filename">garn.ts</div>
          <div className="garn-ts">
            <code className="garn-ts">{garnTs} </code>
          </div>
        </figure>
        <div className="garn-description">
          Your project is configured in Typescript, which provides for great
          discoverability through auto-completion and type errors, as well as
          facilities for abstraction and easy sharing of code. Behind the
          scenes, we translate your Typescript configuration into{" "}
          <a href="https://nixos.org/">Nix</a>.
        </div>

        <div className="garn-description">
          {" "}
          <b>garn</b> currently supports <b>go</b>, <b>javascript</b>, and{" "}
          <b>haskell</b>.
        </div>
      </section>
      <section className="features">
        <h1>features</h1>

        <div className="feature">
          <h2>declarative environments</h2>
          <div className="feature-description">
            Specify your dependencies easily and precisely. Then switch between
            environments with a single command. Unlike containers, <b>garn</b>{" "}
            environments combine well with your personal dev environment.
          </div>
          <Asciinema src={envDemoCastUrl} />
        </div>

        <div className="feature">
          <h2>reproducible builds and tests</h2>
          <div className="feature-description">
            Builds and checks are run in a sandboxed environment, ensuring that
            they are reproducible and deterministic. They can then be run on CI,
            exactly as they are locally, without any additional effort. Results
            can be cached and shared among your CI and local environment.
          </div>
          <Asciinema src={buildDemoCastUrl} />
        </div>

        <div className="feature">
          <h2>a unified interface</h2>
          <div className="feature-description">
            <b>garn</b> provides a unified interface for all your project's
            tests, builds, environments, and executables. It can also serve as
            command documentation, printing help messages that describe exactly
            what commands are available. Instead of wrangling with a host of
            tools, you can focus on getting things done.
          </div>
          <Asciinema src={runDemoCastUrl} rows={25} />
        </div>

        <div className="feature">
          <h2>discoverable Typescript API</h2>
          <div className="feature-description">
            We've carefully designed <b>garn</b> so that you never get stuck.
            After setting up LSP, the available options at any stage can be
            discovered through auto-completion. And unlike YAML files, factoring
            out common functionality, or writing and using libraries, is easy.
            Especially with Deno, which makes imports just a line of code.
          </div>
          <Asciinema src={lspDemoCastUrl} />
        </div>
      </section>
      */}
    </>
  );
};
