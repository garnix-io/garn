import { mkPackage } from "../../../package.ts";
import { nixRaw } from "../../../nix.ts";

/**
 * Fast, compliant alternative implementation of the Python language (2.7)
 */
export const pypy27 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.pypy27`,
  "Fast, compliant alternative implementation of the Python language (2.7)",
);

/**
 * Fast, compliant alternative implementation of the Python language (2.7)
 */
export const pypy27_prebuilt = mkPackage(
  nixRaw`pkgs.pythonInterpreters.pypy27_prebuilt`,
  "Fast, compliant alternative implementation of the Python language (2.7)",
);

/**
 * Fast, compliant alternative implementation of the Python language (3.10)
 */
export const pypy310 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.pypy310`,
  "Fast, compliant alternative implementation of the Python language (3.10)",
);

/**
 * Fast, compliant alternative implementation of the Python language (3.9)
 */
export const pypy39 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.pypy39`,
  "Fast, compliant alternative implementation of the Python language (3.9)",
);

/**
 * Fast, compliant alternative implementation of the Python language (3.9)
 */
export const pypy39_prebuilt = mkPackage(
  nixRaw`pkgs.pythonInterpreters.pypy39_prebuilt`,
  "Fast, compliant alternative implementation of the Python language (3.9)",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python27 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python27`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python310 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python310`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python311 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python311`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python312 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python312`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python313 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python313`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python38 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python38`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python39 = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python39`,
  "A high-level dynamically-typed programming language",
);

/**
 * A high-level dynamically-typed programming language
 */
export const python3Minimal = mkPackage(
  nixRaw`pkgs.pythonInterpreters.python3Minimal`,
  "A high-level dynamically-typed programming language",
);

/**
 * Python 3 interpreter in written Rust
 */
export const rustpython = mkPackage(
  nixRaw`pkgs.pythonInterpreters.rustpython`,
  "Python 3 interpreter in written Rust",
);
