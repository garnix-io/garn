import React from "react";
import "./stylesheet.css";

const ReadMore = () => <a href="./docs/tutorial">Read more...</a>;

export function LandingPageScenario(props: {
  title: string;
  description: React.ReactNode;
  examples: React.ReactNode;
}) {
  return (
    <section className="scenario">
      <div>
        <h1>{props.title}</h1>
        <p>{props.description}</p>
        <div className="examples">
          {props.examples}
        </div>
        <ReadMore />
      </div>
    </section>
  );
}
