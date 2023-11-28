import React from "react";
import { Typography } from "../Typography";
import { Link, NavLink } from "react-router-dom";
import { docMenuItems } from "../../pages/docs";
import "./stylesheet.css";

export function Header() {
  const [menuOpen, setMenuOpen] = React.useState(false);

  React.useEffect(() => {
    const close = () => setMenuOpen(false);
    document.addEventListener("click", close);
    return () => document.removeEventListener("click", close);
  });

  return (
    <header className={`nav ${menuOpen ? "open" : ""}`}>
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
        <hr />
        <DiscordLink />
        <GithubLink />
      </nav>
      <button
        onClick={(e) => {
          e.stopPropagation();
          setMenuOpen(!menuOpen);
        }}
        className="menu-toggle"
      >
        <div />
      </button>
    </header>
  );
}

export const GithubLink = () => (
  <a
    href="https://github.com/garnix-io/garn"
    className="social-link"
    title="View on GitHub"
  >
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 98 96">
      <path
        fillRule="evenodd"
        clipRule="evenodd"
        d="M48.854 0C21.839 0 0 22 0 49.217c0 21.756 13.993 40.172 33.405 46.69 2.427.49 3.316-1.059 3.316-2.362 0-1.141-.08-5.052-.08-9.127-13.59 2.934-16.42-5.867-16.42-5.867-2.184-5.704-5.42-7.17-5.42-7.17-4.448-3.015.324-3.015.324-3.015 4.934.326 7.523 5.052 7.523 5.052 4.367 7.496 11.404 5.378 14.235 4.074.404-3.178 1.699-5.378 3.074-6.6-10.839-1.141-22.243-5.378-22.243-24.283 0-5.378 1.94-9.778 5.014-13.2-.485-1.222-2.184-6.275.486-13.038 0 0 4.125-1.304 13.426 5.052a46.97 46.97 0 0 1 12.214-1.63c4.125 0 8.33.571 12.213 1.63 9.302-6.356 13.427-5.052 13.427-5.052 2.67 6.763.97 11.816.485 13.038 3.155 3.422 5.015 7.822 5.015 13.2 0 18.905-11.404 23.06-22.324 24.283 1.78 1.548 3.316 4.481 3.316 9.126 0 6.6-.08 11.897-.08 13.526 0 1.304.89 2.853 3.316 2.364 19.412-6.52 33.405-24.935 33.405-46.691C97.707 22 75.788 0 48.854 0z"
        fill="#000000"
      />
    </svg>
    <span className="gray">View on GitHub</span>
  </a>
);

export const DiscordLink = () => (
  <a
    href="https://discord.gg/XtDrPsqpVx"
    className="social-link"
    title="Join the Discord Channel"
  >
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="16"
      height="16"
      fill="black"
      viewBox="0.0.17 16"
    >
      <path d="M13.545 2.907a13.227 13.227 0 0 0-3.257-1.011.05.05 0 0 0-.052.025c-.141.25-.297.577-.406.833a12.19 12.19 0 0 0-3.658 0 8.258 8.258 0 0 0-.412-.833.051.051 0 0 0-.052-.025c-1.125.194-2.22.534-3.257 1.011a.041.041 0 0 0-.021.018C.356 6.024-.213 9.047.066 12.032c.001.014.01.028.021.037a13.276 13.276 0 0 0 3.995 2.02.05.05 0 0 0 .056-.019c.308-.42.582-.863.818-1.329a.05.05 0 0 0-.01-.059.051.051 0 0 0-.018-.011 8.875 8.875 0 0 1-1.248-.595.05.05 0 0 1-.02-.066.051.051 0 0 1 .015-.019c.084-.063.168-.129.248-.195a.05.05 0 0 1 .051-.007c2.619 1.196 5.454 1.196 8.041 0a.052.052 0 0 1 .053.007c.08.066.164.132.248.195a.051.051 0 0 1-.004.085 8.254 8.254 0 0 1-1.249.594.05.05 0 0 0-.03.03.052.052 0 0 0 .003.041c.24.465.515.909.817 1.329a.05.05 0 0 0 .056.019 13.235 13.235 0 0 0 4.001-2.02.049.049 0 0 0 .021-.037c.334-3.451-.559-6.449-2.366-9.106a.034.034 0 0 0-.02-.019Zm-8.198 7.307c-.789 0-1.438-.724-1.438-1.612 0-.889.637-1.613 1.438-1.613.807 0 1.45.73 1.438 1.613 0 .888-.637 1.612-1.438 1.612Zm5.316 0c-.788 0-1.438-.724-1.438-1.612 0-.889.637-1.613 1.438-1.613.807 0 1.451.73 1.438 1.613 0 .888-.631 1.612-1.438 1.612Z" />
    </svg>
    <span className="gray">Join the Discord Channel</span>
  </a>
);
