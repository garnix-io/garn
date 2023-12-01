"use client";

import { useEffect, useState } from "react";
import styles from "./styles.module.css";

interface Props {
  prependedText: string;
  texts: string[];
}

export const TypingText = ({ prependedText, texts }: Props) => {
  const [termState, setTermState] = useState({
    deleting: false,
    idx: 0,
    text: "",
  });
  const currentExample = texts[termState.idx];
  useEffect(() => {
    let timeout: NodeJS.Timeout;
    const type = (delay: number, newState: Partial<typeof termState>) => {
      timeout = setTimeout(() => {
        setTermState({
          ...termState,
          ...newState,
        });
      }, delay);
    };
    if (termState.deleting) {
      if (termState.text.length > 0) {
        type(5 + Math.random() * 15, {
          text: currentExample.slice(0, termState.text.length - 1),
        });
      } else {
        type(50, {
          deleting: false,
          idx: (termState.idx + 1) % texts.length,
        });
      }
    } else {
      if (termState.text.length < currentExample.length) {
        type(20 + Math.random() * 45, {
          text: currentExample.slice(0, termState.text.length + 1),
        });
      } else {
        type(2500, { deleting: true });
      }
    }
    return () => clearTimeout(timeout);
  });
  return (
    <span>
      {prependedText} {termState.text}
      <div className={styles.cursor} />
    </span>
  );
};
