import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import { Package } from "./base.ts";
import { dbg, hasTag } from "./utils.ts";
import { NewPackage } from "./package.ts";

export type Executable = {
  tag: "executable";
  description: string;
  nixExpression: string;
};

export const isExecutable = (e: unknown): e is Executable => {
  return hasTag(e, "executable");
};

export const shell = (
  s: TemplateStringsArray,
  ...args: Array<string | Package | NewPackage>
): Executable => {
  const { nixExpression } = serializeNixStr(nixStringFromTemplate(s, ...args));
  return {
    tag: "executable",
    description: `Executes ${nixExpression}`,
    nixExpression,
  };
};

type NixString = {
  template: Array<string>;
  interpolations: Array<string | NixExpression>;
};

type NixExpression = { nixExpression: string };

const nixStringFromTemplate = (
  s: TemplateStringsArray,
  ...interpolations: Array<string | NixExpression>
): NixString => {
  return { template: [...s], interpolations };
};

const serializeNixStr = (s: NixString): NixExpression => {
  const escapedTemplate = s.template.map((part) =>
    ["\\", '"', "$"].reduce(
      (str, char) => str.replaceAll(char, `\\${char}`),
      part
    )
  );
  const nixExpression =
    '"' +
    escapedTemplate.reduce((acc, part, i) => {
      const interpolation = s.interpolations[i];
      switch (typeof interpolation) {
        case "string":
          return acc + part + interpolation;
        case "object":
          return acc + part + "${" + interpolation.nixExpression + "}";
        case "undefined":
          return acc + part;
      }
    }, "") +
    '"';
  return { nixExpression };
};

Deno.test("serializeNixStr correctly serializees into a nix expression", () => {
  assertEquals(
    serializeNixStr(nixStringFromTemplate`foo`).nixExpression,
    '"foo"'
  );
  assertEquals(
    serializeNixStr(nixStringFromTemplate`with ${"string"} interpolation`)
      .nixExpression,
    '"with string interpolation"'
  );
  assertEquals(
    serializeNixStr(
      nixStringFromTemplate`with package ${{
        nixExpression: "pkgs.hello",
      }} works`
    ).nixExpression,
    '"with package ${pkgs.hello} works"'
  );
  assertEquals(
    serializeNixStr(
      nixStringFromTemplate`escaped dollars in strings \${should not interpolate}`
    ).nixExpression,
    '"escaped dollars in strings \\${should not interpolate}"'
  );
  assertEquals(
    serializeNixStr(
      nixStringFromTemplate`"double quotes" are correctly escaped`
    ).nixExpression,
    '"\\"double quotes\\" are correctly escaped"'
  );
});
