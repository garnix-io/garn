import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import {
  oneDark,
  oneLight,
} from "react-syntax-highlighter/dist/esm/styles/prism";

interface Props {
  code: string;
  inverse?: boolean;
  language?: string;
}

export const SampleCode = ({ code, inverse, language }: Props) => {
  return (
    <SyntaxHighlighter
      language={language || "javascript"}
      style={inverse ? oneDark : oneLight}
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
