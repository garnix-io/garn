import { mkPackage } from "../../../package.ts";
import { nixRaw } from "../../../nix.ts";

/**
 * An application for building and managing Phars
 */
export const box = mkPackage(
  nixRaw`pkgs.phpPackages.box`,
  "An application for building and managing Phars",
);

/**
 * DX oriented task runner and command launcher built with PHP
 */
export const castor = mkPackage(
  nixRaw`pkgs.phpPackages.castor`,
  "DX oriented task runner and command launcher built with PHP",
);

/**
 * Dependency Manager for PHP
 */
export const composer = mkPackage(
  nixRaw`pkgs.phpPackages.composer`,
  "Dependency Manager for PHP",
);

/**
 * A deployment tool for PHP
 */
export const deployer = mkPackage(
  nixRaw`pkgs.phpPackages.deployer`,
  "A deployment tool for PHP",
);

/**
 * A PHP code-quality tool
 */
export const grumphp = mkPackage(
  nixRaw`pkgs.phpPackages.grumphp`,
  "A PHP code-quality tool",
);

/**
 * Static analyzer for PHP
 */
export const phan = mkPackage(
  nixRaw`pkgs.phpPackages.phan`,
  "Static analyzer for PHP",
);

/**
 * PHing Is Not GNU make; it's a PHP project build system or build tool based on Apache Ant
 */
export const phing = mkPackage(
  nixRaw`pkgs.phpPackages.phing`,
  "PHing Is Not GNU make; it's a PHP project build system or build tool based on Apache Ant",
);

/**
 * The Phar Installation and Verification Environment (PHIVE)
 */
export const phive = mkPackage(
  nixRaw`pkgs.phpPackages.phive`,
  "The Phar Installation and Verification Environment (PHIVE)",
);

/**
 * A tool to automatically fix PHP coding standards issues
 */
export const php_cs_fixer = mkPackage(
  nixRaw`pkgs.phpPackages.php-cs-fixer`,
  "A tool to automatically fix PHP coding standards issues",
);

/**
 * Tool to check syntax of PHP files faster than serial check with fancier output
 */
export const php_parallel_lint = mkPackage(
  nixRaw`pkgs.phpPackages.php-parallel-lint`,
  "Tool to check syntax of PHP files faster than serial check with fancier output",
);

/**
 * PHP coding standard beautifier and fixer
 */
export const phpcbf = mkPackage(
  nixRaw`pkgs.phpPackages.phpcbf`,
  "PHP coding standard beautifier and fixer",
);

/**
 * PHP coding standard tool
 */
export const phpcs = mkPackage(
  nixRaw`pkgs.phpPackages.phpcs`,
  "PHP coding standard tool",
);

/**
 * PHP code quality analyzer
 */
export const phpmd = mkPackage(
  nixRaw`pkgs.phpPackages.phpmd`,
  "PHP code quality analyzer",
);

/**
 * PHP Static Analysis Tool
 */
export const phpstan = mkPackage(
  nixRaw`pkgs.phpPackages.phpstan`,
  "PHP Static Analysis Tool",
);

/**
 * A static analysis tool for finding errors in PHP applications
 */
export const psalm = mkPackage(
  nixRaw`pkgs.phpPackages.psalm`,
  "A static analysis tool for finding errors in PHP applications",
);

/**
 * PsySH is a runtime developer console, interactive debugger and REPL for PHP.
 */
export const psysh = mkPackage(
  nixRaw`pkgs.phpPackages.psysh`,
  "PsySH is a runtime developer console, interactive debugger and REPL for PHP.",
);
