import React from "react";

export const AsideText: React.FC = (props) => {
  return <aside className="text-aside">{props.children}</aside>;
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
