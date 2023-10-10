import React, { useEffect, useRef } from "react";
import * as AsciinemaPlayer from "asciinema-player";

type AsciinemaProps = {
  src: string
}

export const Asciinema: React.FC<AsciinemaProps> = ({ src }) => {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    AsciinemaPlayer.create(
      src,
      ref.current,
      {
        autoplay: true,
        loop: true,
        theme: "garn",
        terminalFontFamily: "Space Mono",
        controls: false,
        rows: 10
      }
    );
  });
  return (
    <div className="App">
      <div ref={ref}></div>
    </div>
  );
};
