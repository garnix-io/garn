import React from "react";
import { DropdownMenu } from "../components/Dropdown";
import { Tooltip } from "../components/Hovernote";
import { Typography } from "../components/Typography";
import { Asciinema } from "../components/Asciinema";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";
import envDemoCastUrl from "../casts/env-demo.cast";
import placeHolderCastUrl from "../casts/placeholder.cast";

const Keyword: React.FC = props => <span className="keyword">{props.children}</span>;
const String: React.FC = props => <span className="string">{props.children}</span>;
const Export: React.FC = props => <span className="export">{props.children}</span>;

const garnTs = <pre>
        <Keyword>import</Keyword> * as garn from <String>"https://garn.io/ts/v0.0.1/mod.ts"</String>;<br />
        <Keyword>import</Keyword> &#123; <Tooltip item="pkgs">
{`This is a gigantic collection
of packages, nixpkgs. If you
need a tool or dependency,
it's probably here`}
</Tooltip> &#125; from <String>"https://garn.io/ts/v0.0.1/nixpkgs.ts"</String>;<br />
        <br />
        <Export>export</Export> <Keyword>const</Keyword> frontend = garn.javascript.mkNpmProject(&#123; <br />
  {"  "}description: <String>"My npm app"</String>,<br />
  {"  "}<Tooltip item={<>src: <String>"frontend"</String></>}>
{`Supports mono-repos and multiple languages.
`}
  </Tooltip>,<br />
  {"  "}nodeVersion: <String>"18"</String>,<br />
  {"  "}testCommand: <String>"npm run test -- --watchAll=false"</String>,<br />
&#125;).<Tooltip item="withDevTools">
{`withDevTools is all you need
to add a dev tool to your shell.
`}</Tooltip>([pkgs.cowsay]);<br />
        <br />
        <Export>export</Export> <Keyword>const</Keyword> <Tooltip item="backend">
{`const backend: garn.Project<{
  pkg: garn.Package;
  devShell: garn.Environment;
  main: garn.Executable;
}>
A Project is a collection of
packages, environments, and
executables.
`}
        </Tooltip> = garn.go.mkGoProject(&#123; <br />
{"  "}description: <String>"A go server"</String>,<br />
{"  "}moduleName: <String>"server"</String>,<br />
{"  "}src: <String>"backend"</String>,<br />
&#125;);<br />
        </pre>

export const Main: React.FC = () => {
  return (
    <>
      <header>
        <div className="header-title">
          <Typography variant="h1">
            <Link style={{ color: "black" }} to="/">
              garn
            </Link>
          </Typography>
        </div>
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
          Built by <b>garnix</b>.
        </p>
      </footer>
    </>
  );
};

export const Info: React.FC = () => {
  return (
    <>
      <section className="say">
      <Typography variant="h1">what you say...</Typography>

      <figure>
        <div className="filename">garn.ts</div>
        <div className="garn-ts">
          <code className="garn-ts">{garnTs} </code>
        </div>
        <figcaption className="garn-ts">
        <b>garn</b> is configured in Typescript.
        You probably already know it. And anyhow, types
        and tab-completion will guide the way.
            </figcaption>
      </figure>

      <br />
      <br />
      </section>
      <section className="get">
        <Typography variant="h1" className="align-right">...is what you get</Typography>
        <br />


          <div className="feature">
            <h2>Declarative environments</h2>
            <ul>
              <li>Your dependencies go in config file, not in a README </li>
              <li>Makes life easy for your collaborators</li>
              <li>Unlike containers, combines well with your personal dev environment</li>
              <li>Works on Linux and Macs</li>
            </ul>
            <Asciinema src={envDemoCastUrl} />
          </div>

          <div className="feature">
            <h2>Reproducible builds and tests</h2>
            <ul>
              <li>Builds and checks can be run with no further configuration on CI</li>
              <li>No more Heisenbugs</li>
              <li>Because builds are deterministic, they are cacheable, speeding up development and CI</li>
            </ul>
            <Asciinema src={placeHolderCastUrl} />
          </div>


          <div className="feature">
            <h2>A unified interface</h2>
            <ul>
              <li>A CLI that makes the action of your project obvious</li>
              <li>The same commands can run any test in any repo</li>
              <li>Discoverability exactly where you need it</li>
            </ul>
            <Asciinema src="/src/casts/placeholder.cast" />
          </div>


      </section>
    </>
  );
};
