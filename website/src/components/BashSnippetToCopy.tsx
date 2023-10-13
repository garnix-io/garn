import { useState } from "react";
import { CopyToClipboard } from "react-copy-to-clipboard";

export const BashSnippetToCopy = (props: { snippet: string }) => {
  const [copied, setCopied] = useState(false);
  return (
    <div className="bash">
      {props.snippet}
      <CopyToClipboard text={props.snippet} onCopy={() => setCopied(true)}>
        <span className="copy-button">{copied ? "Copied" : "Copy"}</span>
      </CopyToClipboard>
    </div>
  );
};
