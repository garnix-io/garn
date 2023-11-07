import React from "react";
import { Garn } from "../Typography";
import { Link } from "react-router-dom";
import { Code } from "../Code";
import { Tooltip } from "../Hovernote";
import "./stylesheet.css";

const terminalExamples = [
  "check backend.no-todos-allowed",
  "run backend.dev-server",
  "enter backend",
  "build backend",
];

export function Hero() {
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
    <div className="hero">
      <div>
        <h1>garn</h1>
        <h2>Pin down your software</h2>
        <p>Declaratively configure your projects, get reproducible results.</p>
        <div className="ctas">
          <Link to="/docs/tutorial">Read More</Link>
          <Link to="/docs/getting_started">Get Started</Link>
        </div>
      </div>
      <div className="codeblocks">
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
  );
}

const Keyword: React.FC = (props) => (
  <span className="keyword">{props.children}</span>
);
const String: React.FC = (props) => (
  <span className="string">{props.children}</span>
);
const Export: React.FC = (props) => (
  <span className="export">{props.children}</span>
);

const garnTs = (
  <>
    <Keyword>import</Keyword> * as garn from{" "}
    <String>
      "
      <Tooltip item="https://garn.io/ts/v0.0.14/mod.ts">
        <Garn /> uses Deno to run 'garn.ts' files. This means you can import
        dependencies directly through URLs. But you don't need to have Deno
        installed, <Garn /> takes care of that for you.
      </Tooltip>
      "
    </String>
    ;<br />
    <Keyword>import</Keyword> * as{" "}
    <Tooltip item="pkgs">
      {`This is a gigantic collection
of packages, nixpkgs. If you
need a tool or dependency,
it's probably here`}
    </Tooltip>{" "}
    from <String>"https://garn.io/ts/v0.0.14/nixpkgs.ts"</String>;<br />
    <br />
    <Export>export</Export> <Keyword>const</Keyword>{" "}
    <Tooltip item="backend">
      {`A Project is a collection of
packages, environments, and
executables.
`}
    </Tooltip>{" "}
    = garn.go.mkGoProject(&#123; <br />
    {"  "}description: <String>"A Go server"</String>,<br />
    {"  "}src: <String>"."</String>,<br />
    {"  "}goVersion: <String>"1.21"</String>,<br />
    &#125;)
    <br />
    {"  "}.
    <Tooltip item="withDevTools">
      {`withDevTools is all you need
to add a dev tool to your shell.
`}
    </Tooltip>
    ([pkgs.ripgrep, pkgs.air])
    <br />
    {"  "}.
    <Tooltip item="addCheck">
      addCheck adds a <em>pure</em> check that you can run locally or on CI.
    </Tooltip>
    (<String>"no-todos-allowed"</String>, <String>"! rg -i todo ."</String>)
    <br />
    {"  "}.
    <Tooltip item="addExecutable">
      addExecutable adds a script that you can run locally with 'garn run'.
    </Tooltip>
    (<String>"dev-server"</String>, <String>"air main.go"</String>);
  </>
);
