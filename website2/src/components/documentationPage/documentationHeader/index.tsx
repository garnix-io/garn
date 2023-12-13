import { Text } from "@/components/text";
import styles from "./styles.module.css";

export const DocumentationHeader = () => {
  return (
    <section className={styles.container}>
      <div className={styles.content}>
        <Text type="h1">Documentation</Text>
      </div>
    </section>
  );
};
