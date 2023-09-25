import {
  React,
  ReactRouterLink,
  ReactRouterNavLink,
} from "../../deps.client.ts";

/** A wrapper around ReactRouter's Link which seems to have the wrong return type signature for deno */
export function Link(
  props: Parameters<typeof ReactRouterLink>[0],
): React.ReactElement {
  // @ts-expect-error - The deno wrapped version of react router seems to have the wrong return type here
  return <ReactRouterLink {...props} />;
}

/** A wrapper around ReactRouter's NavLink which seems to have the wrong return type signature for deno */
export function NavLink(
  props: Parameters<typeof ReactRouterNavLink>[0],
): React.ReactElement {
  // @ts-expect-error - The deno wrapped version of react router seems to have the wrong return type here
  return <ReactRouterNavLink {...props} />;
}
