import { React, useParams } from "../../deps.client.ts";
import type { DocNode } from "https://deno.land/x/deno_doc@0.67.0/types.d.ts";
import Sidebar from "../components/sidebar.tsx";
import DocNodeDefinition from "../components/ts-doc-node-definition.tsx";

const enum NormalizedKind {
  Function = "function",
  Type = "type",
}

async function fetchDocuments(): Promise<Record<string, Array<DocNode>>> {
  const res = await fetch("/doc.json");
  return await res.json();
}

function normalizeKind(node: DocNode): NormalizedKind | null {
  switch (node.kind) {
    case "variable":
      if (node.variableDef.tsType?.kind === "fnOrConstructor") {
        return NormalizedKind.Function;
      }
      break;
    case "function":
      return NormalizedKind.Function;
    case "typeAlias":
      return NormalizedKind.Type;
  }
  return null;
}

export default function Typescript() {
  const params = useParams();
  const [documents, setDocuments] = React.useState<
    Record<string, Array<DocNode>>
  >({});

  React.useEffect(() => {
    fetchDocuments()
      .then(setDocuments)
      .catch((err) => console.error("Failed to fetch documents", err));
  }, []);

  const selectedNode = documents[params.file || ""]?.find((node) =>
    node.declarationKind === "export" && node.name === params.export
  );

  return (
    <Sidebar
      entries={Object
        .entries(documents)
        .map(([fileName, docNodes]) => ({
          type: "nested",
          text: fileName,
          subEntries: docNodes
            .map((node) => [normalizeKind(node), node] as const)
            .filter(([normalizedKind, node]) =>
              normalizedKind && node.declarationKind === "export"
            )
            .sort(([a], [b]) => a && b && a < b ? -1 : 1)
            .map(([normalizedKind, node]) => ({
              type: "link",
              to: `/typescript/${fileName}/${node.name}`,
              text: `${normalizedKind} ${node.name}`,
            })),
        }))}
    >
      {selectedNode && (
        <div className="page-typescript">
          <h1>{normalizeKind(selectedNode)} {selectedNode.name}</h1>
          <code>
            <DocNodeDefinition node={selectedNode} />
          </code>
          {selectedNode.jsDoc && (
            <div className="jsdoc">
              <h2>Documentation</h2>
              {selectedNode.jsDoc.doc}
            </div>
          )}
        </div>
      )}
    </Sidebar>
  );
}
