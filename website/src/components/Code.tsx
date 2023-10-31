import React from "react";

export function Code(props: {
  header: string;
  lineNumbers?: boolean;
  code: React.ReactNode;
}) {
  return (
    <figure className="codeblock">
      <header>{props.header}</header>
      <code className={props.lineNumbers ? "line-numbers" : undefined}>
        {props.code}
      </code>
    </figure>
  );
}
