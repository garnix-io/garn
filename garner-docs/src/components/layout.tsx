import { React } from "../../deps.client.ts";
import { Link, NavLink } from "./link.tsx";

export default function Layout(props: { children: React.ReactNode }) {
  return (
    <div className="layout">
      <header>
        <Link to="/" className="logo">garner</Link>
        <nav>
          <NavLink to="/typescript">Typescript Docs</NavLink>
        </nav>
      </header>
      <main>{props.children}</main>
    </div>
  );
}
