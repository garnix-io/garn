import * as garn from "http://localhost:8777/mod.ts";
import * as pkgs from "http://localhost:8777/nixpkgs.ts";

export const someproject = garn.python.mkPythonProject({
  src: ".",
  pythonInterpreter: garn.python.interpreters.python310,
});
