import React from "react";
import { CookiesProvider } from "react-cookie";
import { DropdownMenu } from "../components/Dropdown";
import { Typography } from "../components/Typography";
import { Outlet, NavLink, Link } from "react-router-dom";
import { docMenuItems } from "./docs";

export const Main: React.FC = () => {
  return (
    <CookiesProvider>
      <header>
        <div />
        <div className="header-title">
          <div>
            <Typography variant="h1">
              <Link style={{ color: "black" }} to="/">
                {" "}
                garn
              </Link>
            </Typography>
          </div>
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
        <div />
      </article>
      <footer>
        <p>
          {" "}
          Built by <b>garnix</b>.{" "}
        </p>
      </footer>
    </CookiesProvider>
  );
};

export const Info: React.FC = () => {
  return (
    <React.Fragment>
      <Typography variant="h2">just code</Typography>
      <br />
      <Typography variant="body">
        <b>garn</b> is a new build tool and environment manager. You configure
        your project with a <code>garn.ts</code> file that looks like this:
      </Typography>

      <code>
        <pre>
          {`
        import { garner } from "https://garn.io/ts/v0.0.1/mod.ts";
        import { pkgs } from "https://garn.io/ts/v0.0.1/nixpkgs.ts";

        export const frontend = garner.mkNpmFrontend({

        });

        export const backend = garner.mkHaskell({
        });
        `}
        </pre>
      </code>

      <Typography variant="body">
        Then, you can run:
        <ul>
          <li>
            <b>garn enter frontend</b> to enter a shell with all the
            dependencies needed to build the frontend, as well as jq, .
          </li>
          <li>
            <b>garn build backend</b> to build the production Haskell backend.
          </li>
          <li>
            <b>garn run foo</b>.
          </li>
          <li>
            <b>garn check frontend</b> to run <i>reproducible</i>
            tests.
          </li>
        </ul>
        Anyone can get started on the project immediately, without wondering how
        to install dependencies or how to run tests.
      </Typography>

      <br />
    </React.Fragment>
  );
};
