import { React } from "../../deps.client.ts";
import type {
  TsTypeDef,
  TsTypeDefLiteral,
} from "https://deno.land/x/deno_doc@0.67.0/types.d.ts";
import ParameterListAutoIndent from "./param-list-auto-indent.tsx";
import TsFnParam from "./ts-fn-param.tsx";

export default function TsType(props: { type: TsTypeDef }) {
  switch (props.type.kind) {
    case "keyword":
      return <span className="ts-keyword">{props.type.keyword}</span>;
    case "typeRef":
      return (
        <>
          <span className="ts-typeref">{props.type.typeRef.typeName}</span>
          {props.type.typeRef.typeParams && (
            <>
              <ParameterListAutoIndent
                opener="<"
                closer=">"
                list={props.type.typeRef.typeParams.map((typeParam) => (
                  <TsType type={typeParam} />
                ))}
              />
            </>
          )}
        </>
      );
    case "literal":
      return <Literal literal={props.type.literal} />;
    case "union":
      return (
        <ParameterListAutoIndent
          list={props.type.union.map((type) => <TsType type={type} />)}
          joiner=" | "
        />
      );
    case "intersection":
      return (
        <ParameterListAutoIndent
          list={props.type.intersection.map((type) => <TsType type={type} />)}
          joiner=" & "
        />
      );
    case "array":
      return (
        <>
          <TsType type={props.type.array} />[]
        </>
      );
    case "tuple":
      return (
        <ParameterListAutoIndent
          opener="["
          closer="]"
          list={props.type.tuple.map((type) => <TsType type={type} />)}
        />
      );
    case "parenthesized":
      return (
        <>
          (<TsType type={props.type.parenthesized} />)
        </>
      );
    case "typeLiteral":
      return (
        <ParameterListAutoIndent
          opener="{"
          closer="}"
          joiner="; "
          list={props.type.typeLiteral.properties.map((prop) => (
            <>
              {prop.readonly && "readonly "}
              {prop.name}
              {prop.optional && "?"}
              {prop.tsType && (
                <>
                  : <TsType type={prop.tsType} />
                </>
              )}
            </>
          ))}
        />
      );

    case "fnOrConstructor":
      return (
        <ParameterListAutoIndent
          opener="("
          closer={
            <>
              {") => "}
              <TsType type={props.type.fnOrConstructor.tsType} />
            </>
          }
          list={props.type.fnOrConstructor.params.map((param) => (
            <TsFnParam param={param} includeTypeAnnotation={true} />
          ))}
        />
      );

    // These cases are fairly rare so they are left unimplemented for now until we use them
    case "typePredicate":
    case "mapped":
    case "conditional":
    case "importType":
    case "indexedAccess":
    case "infer":
    case "optional":
    case "rest":
    case "this":
    case "typeOperator":
    case "typeQuery":
      return <>unknown</>;
  }
}

function Literal(props: { literal: TsTypeDefLiteral["literal"] }) {
  switch (props.literal.kind) {
    case "string":
      return (
        <span className="ts-literal">
          {JSON.stringify(props.literal.string)}
        </span>
      );
    case "number":
      return (
        <span className="ts-literal">
          {JSON.stringify(props.literal.number)}
        </span>
      );
    case "boolean":
      return (
        <span className="ts-literal">
          {JSON.stringify(props.literal.boolean)}
        </span>
      );
    case "template":
    case "bigInt":
      return <>unknown</>;
  }
}
