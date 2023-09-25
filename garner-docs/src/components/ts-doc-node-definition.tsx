import {
  DocNode,
  DocNodeVariable,
  FunctionDef,
  TsFnOrConstructorDef,
} from "https://deno.land/x/deno_doc@0.67.0/types.d.ts";
import { React } from "../../deps.client.ts";
import TsType from "./ts-type.tsx";
import ParameterListAutoIndent from "./param-list-auto-indent.tsx";
import TsFnParam from "./ts-fn-param.tsx";

export default function DocNodeDefinition({ node }: { node: DocNode }) {
  switch (node.kind) {
    case "variable":
      return <VarNode node={node} />;
    case "function":
      return <FnNode name={node.name} fn={node.functionDef} />;
    case "import":
      return null;
    case "typeAlias":
      return (
        <>
          <span className="ts-keyword">type</span> {node.name} ={" "}
          <TsType type={node.typeAliasDef.tsType} />
        </>
      );

    case "moduleDoc":
    case "enum":
    case "class":
    case "namespace":
    case "interface":
      return <>unknown</>;
  }
}

function VarNode(props: { node: DocNodeVariable }) {
  const { tsType } = props.node.variableDef;
  switch (tsType?.kind) {
    case "fnOrConstructor":
      return (
        <FnNode
          name={props.node.name}
          fn={tsType.fnOrConstructor}
        />
      );
    case "keyword":
    case "literal":
    case "typeRef":
    case "union":
    case "intersection":
    case "array":
    case "tuple":
    case "typeOperator":
    case "parenthesized":
    case "rest":
    case "optional":
    case "typeQuery":
    case "this":
    case "conditional":
    case "importType":
    case "infer":
    case "indexedAccess":
    case "mapped":
    case "typeLiteral":
    case "typePredicate":
    case undefined:
      return <>unknown</>;
  }
}

function FnNode(props: {
  name: string;
  fn: TsFnOrConstructorDef | FunctionDef;
}) {
  const returnType = "tsType" in props.fn
    ? props.fn.tsType
    : props.fn.returnType;
  return (
    <ParameterListAutoIndent
      opener={
        <>
          <span className="ts-keyword">function</span> {props.name}(
        </>
      }
      closer={
        <>
          ): {returnType
            ? <TsType type={returnType} />
            : <span className="ts-keyword">void</span>}
        </>
      }
      list={props.fn.params.map((param) => (
        <TsFnParam param={param} includeTypeAnnotation={true} />
      ))}
    />
  );
}
