import { Link } from "react-router-dom";
import "./stylesheet.css";

export function Header() {
  return (
    <header>
      <section className="desktop">
        <Link to="/">garn</Link>
        <Link to="/documentation">documentation</Link>
      </section>
      <section className="desktop">
        <a href="https://discord.gg/XtDrPsqpVx" target="_blank">
          discord
        </a>
        <a href="https://github.com/garnix-io/garn" target="_blank">
          github
        </a>
      </section>
      <section className="mobile">
        <Link to="/">garn</Link>
        <Link to="/documentation">documentation →</Link>
      </section>
    </header>
  );
}
