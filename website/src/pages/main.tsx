import React from "react";
import { DropdownMenu } from "../components/Dropdown";
import { Tooltip } from "../components/Hovernote";
import { Typography } from "../components/Typography";
import { Asciinema } from "../components/Asciinema";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";

const garnTs = <pre>
        import &#123; garn &#125; from "https://garn.io/ts/v0.0.1/mod.ts";<br />
        import &#123; <Tooltip item="pkgs">
{`This is a gigantic collection
of packages, nixpkgs. If you
need a tool or dependency,
it's probably here`}
</Tooltip> &#125; from "https://garn.io/ts/v0.0.1/nixpkgs.ts";<br />
        <br />
        export const frontend = garn.javascript.mkNpmFrontend(&#123; <br />
  {"  "}description: "My npm app",<br />
  {"  "}<Tooltip item={`src: "frontend"`}>
{`Supports mono-repos and multiple languages.
`}
  </Tooltip>,<br />
  {"  "}nodeVersion: "18",<br />
  {"  "}testCommand: "npm run test -- --watchAll=false",<br />
&#125;).<Tooltip item="withDevTools">
{`withDevTools is all you need
to add a dev tool to your shell.
`}</Tooltip>([pkgs.cowsay]);<br />
        <br />
        export const <Tooltip item="backend">
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
{"  "}description: "A go server",<br />
{"  "}moduleName: "server",<br />
{"  "}src: "backend",<br />
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
            <Asciinema src="src/casts/placeholder.cast" />
          </div>

          <div className="feature">
            <h2>Reproducible builds and tests</h2>
            <ul>
              <li>Builds and checks can be run with no further configuration on CI</li>
              <li>No more Heisenbugs</li>
              <li>Because builds are deterministic, they are cacheable, speeding up development and CI</li>
            </ul>
            <Asciinema src="src/casts/placeholder.cast" />
          </div>


          <div className="feature">
            <h2>A unified interface</h2>
            <ul>
              <li>A CLI that makes the action of your project obvious</li>
              <li>The same commands can run any test in any repo</li>
              <li>Discoverability exactly where you need it</li>
            </ul>
            <Asciinema src="src/casts/placeholder.cast" />
          </div>


      </section>
    </>
  );
};
