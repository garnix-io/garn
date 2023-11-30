import React, { useEffect, useRef } from "react";
import * as AsciinemaPlayer from "asciinema-player";
import "./stylesheet.css";

type AsciinemaProps = {
  src: string;
  rows?: number;
};

export const Asciinema: React.FC<AsciinemaProps> = ({ src, rows }) => {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    AsciinemaPlayer.create(src, ref.current, {
      autoplay: true,
      loop: true,
      theme: "garn",
      terminalFontFamily: "Space Mono",
      controls: false,
      rows: rows || 10,
      cols: 80,
    });
  });
  return (
    <div className="App">
      <div ref={ref}></div>
    </div>
  );
};
