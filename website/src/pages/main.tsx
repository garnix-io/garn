import React from "react";
import { DropdownMenu } from "../components/Dropdown";
import { GithubLink } from "../components/GithubLink";
import { Tooltip } from "../components/Hovernote";
import { Typography } from "../components/Typography";
import { Asciinema } from "../components/Asciinema";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";
import envDemoCastUrl from "../casts/env-demo.cast";
import buildDemoCastUrl from "../casts/build-demo.cast";
import runDemoCastUrl from "../casts/run-demo.cast";
import lspDemoCastUrl from "../casts/lsp-demo.cast";

const Keyword: React.FC = props => <span className="keyword">{props.children}</span>;
const String: React.FC = props => <span className="string">{props.children}</span>;
const Export: React.FC = props => <span className="export">{props.children}</span>;

const garnTs = <pre>
        <Keyword>import</Keyword> * as garn from <String>"https://garn.io/ts/v0.0.11/mod.ts"</String>;<br />
        <Keyword>import</Keyword> * as <Tooltip item="pkgs">
{`This is a gigantic collection
of packages, nixpkgs. If you
need a tool or dependency,
it's probably here`}
</Tooltip> from <String>"https://garn.io/ts/v0.0.11/nixpkgs.ts"</String>;<br />
        <br />
        <Export>export</Export> <Keyword>const</Keyword> frontend = garn.javascript.mkNpmProject(&#123; <br />
  {"  "}description: <String>"My npm app"</String>,<br />
  {"  "}<Tooltip item={<>src: <String>"frontend"</String></>}>
{`Supports mono-repos and multiple languages.`}
  </Tooltip>,<br />
  {"  "}nodeVersion: <String>"18"</String>,<br />
&#125;)<br/>
  {"  "}.<Tooltip item="withDevTools">
{`withDevTools is all you need
to add a dev tool to your shell.
`}</Tooltip>([pkgs.cowsay])<br/>
  {"  "}.<Tooltip item="addCheck">addCheck adds a *pure* check that you can run locally or on CI.</Tooltip>(<String>"run-tests"</String>)<String>`npm run test -- --watchAll=false`</String>;<br />
        <br />
        <Export>export</Export> <Keyword>const</Keyword> <Tooltip item="backend">
{`A Project is a collection of
packages, environments, and
executables.
`}
        </Tooltip> = garn.go.mkGoProject(&#123; <br />
{"  "}description: <String>"A go server"</String>,<br />
{"  "}src: <String>"backend"</String>,<br />
{"  "}goVersion: <String>"1.20"</String>,<br />
&#125;);<br />
        </pre>

export const Main: React.FC = () => {
  return (
    <>
      <header>
        <Typography variant="h1">
          <Link to="/">garn</Link>
        </Typography>
        <GithubLink />
      </header>

      <article>
        <nav>
          <NavLink
            className={({ isActive }) => (isActive ? "black" : "gray")}
            to="/"
          >
            home
          </NavLink>
          <DropdownMenu label="docs" items={docMenuItems} />
        </nav>
        <div className="main-content">
          <Outlet />
        </div>
      </article>
      <footer>
        <p>
          Built by <a href="https://garnix.io">garnix</a>.
        </p>
      </footer>
    </>
  );
};

export const Info: React.FC = () => {
  return (
    <>
      <section className="lede">
          <h1>garn, at your service</h1>
        <div className="garn-description">
          <b>garn</b> is a build tool and environment manager. It can partly or wholly replace manual README instructions, containerization technologies such as docker, package manager such as apt/brew/&c, and command runners such as just.
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
          scenes, we translate your Typescript configuration into <a
          href="https://nixos.org/">Nix</a>.

        </div>

        <div className="garn-description"> <b>garn</b> currently supports <b>go</b>, <b>javascript</b>, and <b>haskell</b>.
        </div>
      </section>
      <section className="features">
          <h1>features</h1>

          <div className="feature">
            <h2>declarative environments</h2>
            <div className="feature-description">
              Specify your dependencies easily and precisely.
              Then switch between environments with a single command. Unlike
              containers, <b>garn</b> environments combine well with your personal
              dev environment.
            </div>
            <Asciinema src={envDemoCastUrl} />
          </div>

          <div className="feature">
            <h2>reproducible builds and tests</h2>
            <div className="feature-description">
            Builds and checks are run in a sandboxed environment, ensuring that
            they are reproducible and deterministic. They can then be run
            on CI, exactly as they are locally, without any additional effort.
            Results can be cached and shared among your CI and local environment.

            </div>
            <Asciinema src={buildDemoCastUrl} />
          </div>


          <div className="feature">
            <h2>a unified interface</h2>
            <div className="feature-description">
            <b>garn</b> provides a unified interface for all your project's
            tests, builds, environments, and executables. It can also serve
            as command documentation, printing help
            messages that describe exactly what commands are available.
            Instead of wrangling with a host of tools, you can focus on getting
            things done.
            </div>
            <Asciinema src={runDemoCastUrl} rows={25}/>
          </div>


          <div className="feature">
            <h2>discoverable Typescript API</h2>
            <div className="feature-description">
            We've carefully designed <b>garn</b> so that you never get stuck.
            After setting up LSP, the available options at any stage can be
            discovered through auto-completion.
            And unlike YAML files, factoring out common functionality, or
            writing and using libraries, is easy. Especially with Deno, which
            makes imports just a line of code.
            </div>
            <Asciinema src={lspDemoCastUrl} />
          </div>


      </section>
    </>
  );
};
