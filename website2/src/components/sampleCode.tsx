import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import { oneLight } from "react-syntax-highlighter/dist/esm/styles/prism";

interface Props {
  code: string;
}

export const SampleCode = ({ code }: Props) => {
  return (
    <SyntaxHighlighter
      language="javascript"
      style={oneLight}
      customStyle={{
        background: "transparent",
        padding: 0,
        paddingBottom: 8,
        margin: 0,
      }}
      codeTagProps={{ style: { background: "transparent" } }}
    >
      {code}
    </SyntaxHighlighter>
  );
};
