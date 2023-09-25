import { ParamDef } from "https://deno.land/x/deno_doc@0.67.0/types.d.ts";
import { React } from "../../deps.client.ts";
import TsType from "./ts-type.tsx";
import ParameterListAutoIndent from "./param-list-auto-indent.tsx";

/** Renders a single function parameter (and optionally its type) */
export default function TsFnParam(props: {
  param: ParamDef;
  includeTypeAnnotation: boolean;
}) {
  const typeAnnotation = props.includeTypeAnnotation && props.param.tsType && (
    <>
      : <TsType type={props.param.tsType} />
    </>
  );

  switch (props.param.kind) {
    case "identifier":
      // e.g. function(a: Type)
      return (
        <>
          {props.param.name}
          {props.param.optional && "?"}
          {typeAnnotation}
        </>
      );
    case "object":
      // e.g. function({ a, b, c }: Type)
      return (
        <span>
          {"{ ... }"}
          {typeAnnotation}
        </span>
      );
    case "array":
      // e.g. function([a, b, c]: Type)
      return (
        <>
          <ParameterListAutoIndent
            opener="["
            closer="]"
            list={props.param.elements.map((el) =>
              el
                ? <TsFnParam param={el} includeTypeAnnotation={false} />
                : <>?</>
            )}
          />
          {typeAnnotation}
        </>
      );
    case "assign":
      // e.g. function(a: Type = b)
      // deno doc currently hard codes the string "UNSUPPORTED" for
      // param.right, so nothing more we can do here (it would be nice to
      // include the default value in the future when deno doc supports it)
      return <TsFnParam param={props.param.left} includeTypeAnnotation />;
    case "rest":
      // e.g. function(...a: Type)
      return (
        <>
          ...<TsFnParam
            param={props.param.arg}
            includeTypeAnnotation={false}
          />
          {typeAnnotation}
        </>
      );
  }
}
