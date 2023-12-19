import Link from "next/link";
import styles from "./styles.module.css";

export const Header = () => {
  return (
    <header className={styles.container}>
      <section className={`${styles.section} ${styles.sectionDesktop}`}>
        <Link className={styles.link} href="/">
          garn
        </Link>
        <Link className={styles.link} href="/docs/getting_started">
          documentation
        </Link>
      </section>
      <section className={`${styles.section} ${styles.sectionDesktop}`}>
        <Link
          className={styles.link}
          href="https://discord.gg/XtDrPsqpVx"
          target="_blank"
        >
          discord
        </Link>
        <Link
          className={styles.link}
          href="https://github.com/garnix-io/garn"
          target="_blank"
        >
          github
        </Link>
      </section>
      <section className={`${styles.section} ${styles.sectionMobile}`}>
        <Link className={styles.link} href="/">
          garn
        </Link>
        <Link className={styles.link} href="/documentation">
          documentation &rarr;
        </Link>
      </section>
    </header>
  );
};
