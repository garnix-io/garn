import { mkPackage } from "../../../package.ts";
import { nixRaw } from "../../../nix.ts";

/**
 * Downloads your Rust project's dependencies and builds your project
 */
export const cargo = mkPackage(
  nixRaw`pkgs.rustPackages.cargo`,
  "Downloads your Rust project's dependencies and builds your project",
);

/**
 * A tool to make production Rust binaries auditable
 */
export const cargo_auditable = mkPackage(
  nixRaw`pkgs.rustPackages.cargo-auditable`,
  "A tool to make production Rust binaries auditable",
);

/**
 * A tool to make production Rust binaries auditable
 */
export const cargo_auditable_cargo_wrapper = mkPackage(
  nixRaw`pkgs.rustPackages.cargo-auditable-cargo-wrapper`,
  "A tool to make production Rust binaries auditable",
);

/**
 * A bunch of lints to catch common mistakes and improve your Rust code
 */
export const clippy = mkPackage(
  nixRaw`pkgs.rustPackages.clippy`,
  "A bunch of lints to catch common mistakes and improve your Rust code",
);

/**
 * A safe, concurrent, practical language
 */
export const rustc = mkPackage(
  nixRaw`pkgs.rustPackages.rustc`,
  "A safe, concurrent, practical language",
);

/**
 * A tool for formatting Rust code according to style guidelines
 */
export const rustfmt = mkPackage(
  nixRaw`pkgs.rustPackages.rustfmt`,
  "A tool for formatting Rust code according to style guidelines",
);
