import { mkPackage } from "../../../package.ts";
import { nixRaw } from "../../../nix.ts";

/**
 * Console ascii line graphs in pure Nim
 */
export const asciigraph = mkPackage(
  nixRaw`pkgs.nimPackages.asciigraph`,
  "Console ascii line graphs in pure Nim",
);

export const astpatternmatching = mkPackage(
  nixRaw`pkgs.nimPackages.astpatternmatching`,
  "",
);

/**
 * Various asynchronous tools for Nim language
 */
export const asynctools = mkPackage(
  nixRaw`pkgs.nimPackages.asynctools`,
  "Various asynchronous tools for Nim language",
);

/**
 * Nim package cloner
 */
export const atlas = mkPackage(
  nixRaw`pkgs.nimPackages.atlas`,
  "Nim package cloner",
);

/**
 * Base32 library for Nim
 */
export const base32 = mkPackage(
  nixRaw`pkgs.nimPackages.base32`,
  "Base32 library for Nim",
);

/**
 * Base45 library for Nim
 */
export const base45 = mkPackage(
  nixRaw`pkgs.nimPackages.base45`,
  "Base45 library for Nim",
);

/**
 * 2d collision library
 */
export const bumpy = mkPackage(
  nixRaw`pkgs.nimPackages.bumpy`,
  "2d collision library",
);

/**
 * Tool to translate Ansi C code to Nim
 */
export const c2nim = mkPackage(
  nixRaw`pkgs.nimPackages.c2nim`,
  "Tool to translate Ansi C code to Nim",
);

/**
 * Concise Binary Object Representation decoder and encoder (RFC8949)
 */
export const cbor = mkPackage(
  nixRaw`pkgs.nimPackages.cbor`,
  "Concise Binary Object Representation decoder and encoder (RFC8949)",
);

/**
 * Everything you want to do with colors
 */
export const chroma = mkPackage(
  nixRaw`pkgs.nimPackages.chroma`,
  "Everything you want to do with colors",
);

/**
 * Nim implementation of the Constrained Application Protocol (CoAP) over TCP
 */
export const coap = mkPackage(
  nixRaw`pkgs.nimPackages.coap`,
  "Nim implementation of the Constrained Application Protocol (CoAP) over TCP",
);

/**
 * Manage CSV files easily in Nim
 */
export const csvtools = mkPackage(
  nixRaw`pkgs.nimPackages.csvtools`,
  "Manage CSV files easily in Nim",
);

/**
 * Unified db connector in Nim
 */
export const db_connector = mkPackage(
  nixRaw`pkgs.nimPackages.db_connector`,
  "Unified db connector in Nim",
);

export const docopt = mkPackage(
  nixRaw`pkgs.nimPackages.docopt`,
  "",
);

export const eris = mkPackage(
  nixRaw`pkgs.nimPackages.eris`,
  "",
);

/**
 * Tools and serializer for plain flat binary files
 */
export const flatty = mkPackage(
  nixRaw`pkgs.nimPackages.flatty`,
  "Tools and serializer for plain flat binary files",
);

/**
 * Some Nim procedures for looking up freedesktop.org data
 */
export const freedesktop_org = mkPackage(
  nixRaw`pkgs.nimPackages.freedesktop_org`,
  "Some Nim procedures for looking up freedesktop.org data",
);

export const frosty = mkPackage(
  nixRaw`pkgs.nimPackages.frosty`,
  "",
);

/**
 * Nim wrapper over the getdns library
 */
export const getdns = mkPackage(
  nixRaw`pkgs.nimPackages.getdns`,
  "Nim wrapper over the getdns library",
);

/**
 * Hash Library for Nim
 */
export const hashlib = mkPackage(
  nixRaw`pkgs.nimPackages.hashlib`,
  "Hash Library for Nim",
);

/**
 * Nim wrapper for htslib for parsing genomics data files
 */
export const hts = mkPackage(
  nixRaw`pkgs.nimPackages.hts`,
  "Nim wrapper for htslib for parsing genomics data files",
);

/**
 * A curses inspired simple cross-platform console library for Nim
 */
export const illwill = mkPackage(
  nixRaw`pkgs.nimPackages.illwill`,
  "A curses inspired simple cross-platform console library for Nim",
);

/**
 * Mouse enabled widgets for illwill
 */
export const illwillwidgets = mkPackage(
  nixRaw`pkgs.nimPackages.illwillwidgets`,
  "Mouse enabled widgets for illwill",
);

export const jester = mkPackage(
  nixRaw`pkgs.nimPackages.jester`,
  "",
);

/**
 * Schema validation of JSON for Nim
 */
export const jsonschema = mkPackage(
  nixRaw`pkgs.nimPackages.jsonschema`,
  "Schema validation of JSON for Nim",
);

/**
 * A loose, direct to object json parser with hooks
 */
export const jsony = mkPackage(
  nixRaw`pkgs.nimPackages.jsony`,
  "A loose, direct to object json parser with hooks",
);

export const karax = mkPackage(
  nixRaw`pkgs.nimPackages.karax`,
  "",
);

export const lscolors = mkPackage(
  nixRaw`pkgs.nimPackages.lscolors`,
  "",
);

export const markdown = mkPackage(
  nixRaw`pkgs.nimPackages.markdown`,
  "",
);

/**
 * Statically typed, imperative programming language (x86_64-unknown-linux-gnu wrapper)
 */
export const nim = mkPackage(
  nixRaw`pkgs.nimPackages.nim`,
  "Statically typed, imperative programming language (x86_64-unknown-linux-gnu wrapper)",
);

/**
 * Secure Hash Algorithm 2
 */
export const nimSHA2 = mkPackage(
  nixRaw`pkgs.nimPackages.nimSHA2`,
  "Secure Hash Algorithm 2",
);

/**
 * Internal Nixpkgs utility for buildNimPackage.
 */
export const nim_builder = mkPackage(
  nixRaw`pkgs.nimPackages.nim_builder`,
  "Internal Nixpkgs utility for buildNimPackage.",
);

/**
 * Package manager for the Nim programming language
 */
export const nimble = mkPackage(
  nixRaw`pkgs.nimPackages.nimble`,
  "Package manager for the Nim programming language",
);

export const nimbox = mkPackage(
  nixRaw`pkgs.nimPackages.nimbox`,
  "",
);

export const nimcrypto = mkPackage(
  nixRaw`pkgs.nimPackages.nimcrypto`,
  "",
);

/**
 * The Ultimate Raylib gaming library wrapper for Nim
 */
export const nimraylib_now = mkPackage(
  nixRaw`pkgs.nimPackages.nimraylib-now`,
  "The Ultimate Raylib gaming library wrapper for Nim",
);

/**
 * Pleasant Nim bindings for SIMD instruction sets
 */
export const nimsimd = mkPackage(
  nixRaw`pkgs.nimPackages.nimsimd`,
  "Pleasant Nim bindings for SIMD instruction sets",
);

/**
 * Nim implementation of linenoise
 */
export const noise = mkPackage(
  nixRaw`pkgs.nimPackages.noise`,
  "Nim implementation of linenoise",
);

/**
 * NPeg is a pure Nim pattern matching library
 */
export const npeg = mkPackage(
  nixRaw`pkgs.nimPackages.npeg`,
  "NPeg is a pure Nim pattern matching library",
);

export const packedjson = mkPackage(
  nixRaw`pkgs.nimPackages.packedjson`,
  "",
);

/**
 * A Nim library to parse TOML files
 */
export const parsetoml = mkPackage(
  nixRaw`pkgs.nimPackages.parsetoml`,
  "A Nim library to parse TOML files",
);

/**
 * Full-featured 2d graphics library for Nim
 */
export const pixie = mkPackage(
  nixRaw`pkgs.nimPackages.pixie`,
  "Full-featured 2d graphics library for Nim",
);

/**
 * Nim implementation of the Preserves data language
 */
export const preserves = mkPackage(
  nixRaw`pkgs.nimPackages.preserves`,
  "Nim implementation of the Preserves data language",
);

export const redis = mkPackage(
  nixRaw`pkgs.nimPackages.redis`,
  "",
);

export const redpool = mkPackage(
  nixRaw`pkgs.nimPackages.redpool`,
  "",
);

/**
 * Pure Nim regex engine
 */
export const regex = mkPackage(
  nixRaw`pkgs.nimPackages.regex`,
  "Pure Nim regex engine",
);

/**
 * Nim wrapper for RocksDB
 */
export const rocksdb = mkPackage(
  nixRaw`pkgs.nimPackages.rocksdb`,
  "Nim wrapper for RocksDB",
);

/**
 * safeseq library for nim
 */
export const safeseq = mkPackage(
  nixRaw`pkgs.nimPackages.safeseq`,
  "safeseq library for nim",
);

/**
 * safeset library for nim
 */
export const safeset = mkPackage(
  nixRaw`pkgs.nimPackages.safeset`,
  "safeset library for nim",
);

export const sass = mkPackage(
  nixRaw`pkgs.nimPackages.sass`,
  "",
);

/**
 * Nim wrapper for SDL 2.x
 */
export const sdl2 = mkPackage(
  nixRaw`pkgs.nimPackages.sdl2`,
  "Nim wrapper for SDL 2.x",
);

/**
 * Unicode text segmentation (tr29)
 */
export const segmentation = mkPackage(
  nixRaw`pkgs.nimPackages.segmentation`,
  "Unicode text segmentation (tr29)",
);

/**
 * SMTP client
 */
export const smtp = mkPackage(
  nixRaw`pkgs.nimPackages.smtp`,
  "SMTP client",
);

/**
 * Nim implementation of snappy compression algorithm
 */
export const snappy = mkPackage(
  nixRaw`pkgs.nimPackages.snappy`,
  "Nim implementation of snappy compression algorithm",
);

/**
 * A Smalltalk and Rebol inspired language implemented as an AST interpreter in Nim
 */
export const spry = mkPackage(
  nixRaw`pkgs.nimPackages.spry`,
  "A Smalltalk and Rebol inspired language implemented as an AST interpreter in Nim",
);

/**
 * Spry virtual machine
 */
export const spryvm = mkPackage(
  nixRaw`pkgs.nimPackages.spryvm`,
  "Spry virtual machine",
);

/**
 * Backports, standard library candidates and small utilities that don't yet deserve their own repository
 */
export const stew = mkPackage(
  nixRaw`pkgs.nimPackages.stew`,
  "Backports, standard library candidates and small utilities that don't yet deserve their own repository",
);

export const supersnappy = mkPackage(
  nixRaw`pkgs.nimPackages.supersnappy`,
  "",
);

/**
 * Nim implementation of the Syndicated Actor model
 */
export const syndicate = mkPackage(
  nixRaw`pkgs.nimPackages.syndicate`,
  "Nim implementation of the Syndicated Actor model",
);

/**
 * Transport Services Interface
 */
export const taps = mkPackage(
  nixRaw`pkgs.nimPackages.taps`,
  "Transport Services Interface",
);

/**
 * Temporary files and folders
 */
export const tempfile = mkPackage(
  nixRaw`pkgs.nimPackages.tempfile`,
  "Temporary files and folders",
);

/**
 * Nim wrappers over some of the Tkrzw C++ library
 */
export const tkrzw = mkPackage(
  nixRaw`pkgs.nimPackages.tkrzw`,
  "Nim wrappers over some of the Tkrzw C++ library",
);

/**
 * Nim bindings to libui
 */
export const ui = mkPackage(
  nixRaw`pkgs.nimPackages.ui`,
  "Nim bindings to libui",
);

/**
 * Unicode Character Database (UCD, tr44) for Nim
 */
export const unicodedb = mkPackage(
  nixRaw`pkgs.nimPackages.unicodedb`,
  "Unicode Character Database (UCD, tr44) for Nim",
);

/**
 * Common unicode operations
 */
export const unicodeplus = mkPackage(
  nixRaw`pkgs.nimPackages.unicodeplus`,
  "Common unicode operations",
);

/**
 * Math vector library for graphical things
 */
export const vmath = mkPackage(
  nixRaw`pkgs.nimPackages.vmath`,
  "Math vector library for graphical things",
);

/**
 * Simple WebSocket library for Nim
 */
export const ws = mkPackage(
  nixRaw`pkgs.nimPackages.ws`,
  "Simple WebSocket library for Nim",
);

/**
 * X11 library for nim
 */
export const x11 = mkPackage(
  nixRaw`pkgs.nimPackages.x11`,
  "X11 library for nim",
);

/**
 * Pure Nim implementation of deflate, zlib, gzip and zip
 */
export const zippy = mkPackage(
  nixRaw`pkgs.nimPackages.zippy`,
  "Pure Nim implementation of deflate, zlib, gzip and zip",
);
