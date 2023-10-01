import * as haskell from "./haskell.ts";
import * as typescript from "./haskell.ts";
import { initialize } from "./initializer.ts";

const initializers = Array.prototype.concat(
  haskell.initializers,
  typescript.initializers
);

initialize(initializers)
