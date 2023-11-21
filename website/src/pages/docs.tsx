import React from "react";
import { useParams } from "react-router-dom";

type Info = {
  index: number;
  url: string;
  name: string;
};

const docEntries: (readonly [Info, React.ReactElement])[] = Object.values(
  import.meta.glob("../docs/*.mdx", { eager: true }) as Record<
    string,
    { info: Info; default: React.FC }
  >,
)
  .sort((a, b) => +a.info.index - +b.info.index)
  .map(({ info, default: Component }) => [info, <Component />] as const);

export const docMenuItems: { name: string; url: string }[] = [
  ...docEntries.map(([{ name, url }]) => ({ name, url: `/docs/${url}` })),
  {
    name: "typescript api",
    url: "https://doc.deno.land/https://garn.io/ts/v0.0.16/mod.ts",
  },
];

export const Docs: React.FC<{ is_index?: boolean }> = () => {
  const params = useParams();
  const el = docEntries.find(([info, _]) => info.url == params.docItem);
  if (el === undefined) {
    return (
      <div>Could not find what you're looking for. Please check the URL.</div>
    );
  }
  return <div className="docs">{el[1]}</div>;
};
