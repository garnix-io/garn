import { React } from "../../deps.client.ts";
import { Link } from "./link.tsx";

type SideBarEntry =
  | { type?: "link"; to: string; text: string }
  | { type: "nested"; text: string; subEntries: Array<SideBarEntry> };

export default function Sidebar(props: {
  entries: Array<SideBarEntry>;
  children: React.ReactNode;
}) {
  return (
    <div className="sidebar">
      <aside>
        <ul>
          {props.entries.map((entry) => <SidebarListItem entry={entry} />)}
        </ul>
      </aside>
      <section>
        {props.children}
      </section>
    </div>
  );
}

function SidebarListItem(props: {
  entry: SideBarEntry;
}) {
  switch (props.entry.type) {
    case undefined:
    case "link":
      return (
        <li>
          <Link to={props.entry.to}>{props.entry.text}</Link>
        </li>
      );
    case "nested":
      return (
        <li>
          {props.entry.text}
          <ul>
            {props.entry.subEntries.map((entry) => (
              <SidebarListItem entry={entry} />
            ))}
          </ul>
        </li>
      );
  }
}
