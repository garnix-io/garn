import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";
import { hasTag } from "./utils.ts";
import { Package } from "./package.ts";

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
  ...args: Array<string | Package>
): Executable => {
  const shellScript = serializeNixStr(nixStringFromTemplate(s, ...args));
  return {
    tag: "executable",
    description: `Executes ${shellScript}`,
    nixExpression: `"\${pkgs.writeScriptBin "executable" ${shellScript.nixExpression}}/bin/executable"`,
  };
};

type NixString = {
  template: Array<string>;
  interpolations: Array<string | NixExpression>;
};

type NixExpression = { nixExpression: string };

export const nixStringFromTemplate = (
  s: TemplateStringsArray,
  ...interpolations: Array<string | NixExpression>
): NixString => {
  return { template: [...s], interpolations };
};

export const serializeNixStr = (s: NixString): NixExpression => {
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

export const concat = (a: NixString, b: NixString): NixString => {
  return {
    template: [
      ...a.template.slice(0, -1),
      a.template[a.template.length - 1] + b.template[0],
      ...b.template.slice(1),
    ],
    interpolations: [...a.interpolations, ...b.interpolations],
  };
};

Deno.test("concat correctly concatenates nix expressions", () => {
  const A: NixExpression = { nixExpression: "a" };
  const B: NixExpression = { nixExpression: "b" };
  const C: NixExpression = { nixExpression: "c" };
  assertEquals(
    concat(
      nixStringFromTemplate`foo${A}bar${B}baz`,
      nixStringFromTemplate`fizz${C}buzz`
    ),
    {
      template: ["foo", "bar", "bazfizz", "buzz"],
      interpolations: [A, B, C],
    }
  );
});

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
