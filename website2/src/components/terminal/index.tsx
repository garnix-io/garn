import { ReactNode } from "react";
import styles from "./styles.module.css";

interface Props {
  title: string;
  text: ReactNode;
}

export const Terminal = ({ title, text }: Props) => {
  return (
    <div className={styles.container}>
      <div className={styles.header}>{title}</div>
      <pre className={styles.content}>{text}</pre>
    </div>
  );
};
