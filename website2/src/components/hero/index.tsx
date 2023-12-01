import Link from "next/link";
import styles from "./styles.module.css";
import { TerminalHeader } from "../terminalHeader";
import { TypingText } from "../typingText";

const TERMINAL_EXAMPLES = [
  "check backend.no-todos-allowed",
  "run backend.dev-server",
  "enter backend",
  "build backend",
];

export const Hero = () => {
  return (
    <section className={styles.container}>
      <div className={styles.content}>
        <div className={styles.header}>
          Declaratively configure your projects, get reproducible results
        </div>
        <div className={styles.actions}>
          <Link className={styles.action} href="/documentation/get-started">
            <div>Get started</div>
            <div>&rarr;</div>
          </Link>
          <Link className={styles.action} href="/documentation/read-more">
            <div>READ MORE</div>
            <div>&rarr;</div>
          </Link>
        </div>
        <TerminalHeader title="garn.ts" />
        <div className={styles.terminal}></div>
        <TerminalHeader title="terminal" />
        <div className={styles.terminal}>
          <TypingText prependedText="$ garn" texts={TERMINAL_EXAMPLES} />
        </div>
      </div>
    </section>
  );
};
