import styles from "./styles.module.css";

interface Props {
  title: string;
}

export const TerminalHeader = ({ title }: Props) => {
  return <div className={styles.container}>{title}</div>;
};
