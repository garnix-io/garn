"use client";

import { useEffect, useState } from "react";
import styles from "./styles.module.css";

export type Command = TextCommand | OtherCommand;

interface TextCommand extends BasicCommand {
  action: "type" | "response";
  text: string;
}

interface OtherCommand extends BasicCommand {
  action: "clear" | "restart";
}

interface BasicCommand {
  delay?: number;
}

interface Props {
  prependedText: string;
  commands: Command[];
}

export const TypingText2 = ({ prependedText, commands }: Props) => {
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
    const type = (delay: number, newState: Partial<typeof termState>) => {
      timeout = setTimeout(() => {
        setTermState({
          ...termState,
          ...newState,
        });
      }, delay);
    };
    if (currentCommand.action === "type") {
      if (termState.currentLine.length === 0)
        type(currentCommand.delay || 50, {
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
        type(20 + Math.random() * 45, {
          currentLine: `${termState.currentLine}${currentChar}`,
        });
      } else {
        type(100, {
          idx: (termState.idx += 1),
          currentLine: "",
          lines: [...termState.lines, termState.currentLine],
        });
      }
    } else if (currentCommand.action === "response") {
      type(currentCommand.delay || 100, {
        lines: [...termState.lines, currentCommand.text],
        idx: (termState.idx += 1),
      });
    } else if (currentCommand.action === "clear") {
      type(currentCommand.delay || 100, {
        lines: [],
        idx: (termState.idx += 1),
      });
    } else if (currentCommand.action === "restart") {
      type(currentCommand.delay || 100, { idx: 0 });
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
      <div className={styles.cursor} />
    </pre>
  );
};
