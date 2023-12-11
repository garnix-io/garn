import { useState, useEffect } from "react";
import { CopyToClipboard } from "react-copy-to-clipboard";

export const BashSnippetToCopy = (props: { snippet: string }) => {
  const [copied, setCopied] = useState(false);
  useEffect(() => {
    if (copied) {
      const timeout = setTimeout(() => setCopied(false), 2000);
      return () => clearTimeout(timeout);
    }
  }, [copied]);
  return (
    <div className="bash-snippet-to-copy">
      <code>{props.snippet}</code>
      <CopyToClipboard
        text={props.snippet}
        onCopy={() => {
          plausible?.("click#bash-snippet-copy");
          setCopied(true);
        }}
      >
        <button role="button">{copied ? "Copied" : "Copy"}</button>
      </CopyToClipboard>
    </div>
  );
};
