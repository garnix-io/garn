"use client";

import Image from "next/image";
import { useWindowSize } from "usehooks-ts";
import { HeaderWithActions } from "@/components/headerWithActions";
import backgroundShape from "@/../public/shape.svg";
import styles from "./styles.module.css";

export const Social = () => {
  const { width } = useWindowSize();
  console.log(width);
  return (
    <section className={styles.container}>
      <HeaderWithActions
        className={styles.header}
        title="Experience real connections and support in our virtual talk circles"
        links={[
          {
            text: "Discord",
            href: "https://discord.gg/XtDrPsqpVx",
          },
          {
            text: "Github",
            href: "https://github.com/garnix-io/garn",
          },
        ]}
      />
      <Image
        src={backgroundShape}
        alt="social background d20"
        className={styles.backgroundShape}
        fill={width <= 850}
      />
    </section>
  );
};
