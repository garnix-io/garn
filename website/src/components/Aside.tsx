import React from "react";

type AsideTextProps = {
  type?: "warning" | "hint"
}

export const AsideText: React.FC<AsideTextProps> = (props) => {
  const header = props.type !== undefined ? (<h3 className="aside-type">{props.type}</h3>) : (<div/>)
  return (
    <aside className="text-aside">
      {header}
      <div className="aside-content">{props.children}</div>
    </aside>
    );
};


interface AsideImgProps {
  src: string;
}

export const AsideImg: React.FC<AsideImgProps> = (props) => {
  return (
    <aside className="img-aside">
      <img src={props.src} />
    </aside>
  );
};
