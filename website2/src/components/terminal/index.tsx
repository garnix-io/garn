import { ReactNode } from "react";
import styles from "./styles.module.css";

interface Props {
  title?: string;
  text: ReactNode;
  expectedLineCount?: number;
  inverse?: boolean;
  className?: string;
}

export const Terminal = ({
  title,
  text,
  expectedLineCount,
  inverse,
  className,
}: Props) => {
  return (
    <div
      className={`${styles.container} ${
        inverse ? styles.inverse : ""
      } ${className}`}
    >
      {title && <div className={styles.header}>{title}</div>}
      <pre
        className={styles.content}
        style={expectedLineCount ? { minHeight: expectedLineCount * 28 } : {}}
      >
        {text}
      </pre>
    </div>
  );
};
