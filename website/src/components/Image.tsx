import React from "react";

export function Image(props: { src: string; header: string }) {
  return (
    <figure className="codeblock">
      <header>{props.header}</header>
      <img src={props.src} />;
    </figure>
  );
}
