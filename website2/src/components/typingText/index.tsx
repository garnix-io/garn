"use client";

import { useEffect, useState } from "react";
import styles from "./styles.module.css";

export type Command = TextCommand | OtherCommand;

type TextCommand = BasicCommand & {
  action: "type" | "response";
  text: string;
};

type OtherCommand = BasicCommand & {
  action: "clear" | "restart" | "delete" | "delay";
};

type BasicCommand = {
  delay?: number;
  showCursor?: boolean;
};

type Props = {
  prependedText: string;
  commands: Command[];
};

export const TypingText = ({ prependedText, commands }: Props) => {
  const [termState, setTermState] = useState<{
    idx: number;
    lines: string[];
    currentLine: string;
  }>({
    idx: 0,
    lines: [],
    currentLine: "",
  });
  const currentCommand = commands[termState.idx];
  useEffect(() => {
    let timeout: NodeJS.Timeout;
    const delayedUpdate = (
      delay: number,
      newState: Partial<typeof termState>
    ) => {
      timeout = setTimeout(() => {
        setTermState({
          ...termState,
          ...newState,
        });
      }, delay);
    };
    if (currentCommand.action === "delay") {
      delayedUpdate(currentCommand.delay || 1000, {
        idx: (termState.idx += 1),
      });
    } else if (currentCommand.action === "type") {
      if (termState.currentLine.length === 0)
        delayedUpdate(currentCommand.delay || 50, {
          currentLine: `${prependedText} `,
        });
      else if (
        termState.currentLine.length <
        currentCommand.text.length + prependedText.length + 1
      ) {
        const currentChar = currentCommand.text.slice(
          termState.currentLine.length - prependedText.length - 1,
          termState.currentLine.length - prependedText.length
        );
        delayedUpdate(20 + Math.random() * 45, {
          currentLine: `${termState.currentLine}${currentChar}`,
        });
      } else {
        delayedUpdate(100, {
          idx: (termState.idx += 1),
          currentLine: "",
          lines: [...termState.lines, termState.currentLine],
        });
      }
    } else if (currentCommand.action === "delete") {
      if (termState.currentLine.length === 0) {
        delayedUpdate(currentCommand.delay || 50, {
          lines: termState.lines.slice(0, termState.lines.length - 1),
          currentLine: termState.lines[termState.lines.length - 1],
        });
      } else if (termState.currentLine.length <= prependedText.length + 1)
        delayedUpdate(100, { idx: (termState.idx += 1) });
      else
        delayedUpdate(5 + Math.random() * 15, {
          currentLine: termState.currentLine.slice(
            0,
            termState.currentLine.length - 1
          ),
        });
    } else if (currentCommand.action === "response") {
      delayedUpdate(currentCommand.delay || 100, {
        lines: [...termState.lines, currentCommand.text],
        idx: (termState.idx += 1),
      });
    } else if (currentCommand.action === "clear") {
      delayedUpdate(currentCommand.delay || 100, {
        lines: [],
        idx: (termState.idx += 1),
      });
    } else if (currentCommand.action === "restart") {
      delayedUpdate(currentCommand.delay || 100, { idx: 0 });
    }
    return () => clearTimeout(timeout);
  });
  const lines =
    termState.currentLine.length > 0
      ? [...termState.lines, termState.currentLine]
      : termState.lines;
  return (
    <pre className={styles.container}>
      {lines.join("\n")}
      {((["type", "delete"].includes(currentCommand.action) &&
        termState.currentLine.length > 0) ||
        currentCommand.showCursor) && <div className={styles.cursor} />}
    </pre>
  );
};
