import React from "react";
import { DiscordLink, GithubLink } from "../components/HeaderLinks";
import { Tooltip } from "../components/Hovernote";
import { Typography } from "../components/Typography";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";
import { Code } from "../components/Code";

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
      <section>
        <h1>
          Have you ever worked on a project where you had the wrong version of
          node installed? Or golang? Or something else?
        </h1>
        <p>
          <Garn /> manages all of your project dependencies in encapsulated
          environments. <a>Read more</a>
          <div className="more">
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
          </div>
        </p>
      </section>
    </>
  );
};
