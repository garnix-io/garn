import styles from "./styles.module.css";
import { Text } from "../text";
import Link from "next/link";

export const Footer = () => {
  return (
    <footer className={styles.container}>
      <Text type="h3">Built by</Text>
      <Link href="https://garnix.io/" target="_blank">
        <Text type="h3" className={styles.garnix}>
          Garnix
        </Text>
      </Link>
    </footer>
  );
};
