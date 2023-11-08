// This is in its own file because the `with { type: "json" }` syntax breaks code formatting.
import version from "./version.json" with { type: "json" };

export const GARN_TS_LIB_VERSION = version.tsLibVersion;
