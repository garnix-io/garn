import { ReactNode } from "react";
import styles from "./styles.module.css";

interface Props {
  title: string;
  text: ReactNode;
  expectedLineCount?: number;
}

export const Terminal = ({ title, text, expectedLineCount }: Props) => {
  return (
    <div className={styles.container}>
      <div className={styles.header}>{title}</div>
      <pre
        className={styles.content}
        style={expectedLineCount ? { minHeight: expectedLineCount * 28 } : {}}
      >
        {text}
      </pre>
    </div>
  );
};
