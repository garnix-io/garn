import { Text } from "../text";
import { SampleCode } from "../sampleCode";
import { Terminal } from "../terminal";
import styles from "./styles.module.css";
import { Command, TypingText } from "../typingText";

const CODE = `import * as garn from "https://garn.io/ts/v0.0.14/mod.ts";

export const frontend = garn.javascript.mkNpmProject({
  description: "my frontend project",
  src: ".",
  nodeVersion: "18",
});`;

const COMMANDS: Command[] = [
  { action: "type", text: "node" },
  { action: "delay", delay: 300, showCursor: true },
  { action: "response", text: "node: command not found" },
  { action: "delay", delay: 1000 },
  { action: "type", text: "garn enter frontend" },
  { action: "delay", delay: 300, showCursor: true },
  {
    action: "response",
    text: "[garn] Entering frontend shell. Type 'exit' to exit.",
  },
  { action: "delay", delay: 1000 },
  { action: "type", text: "node --version" },
  { action: "delay", delay: 300, showCursor: true },
  { action: "response", text: "v18.17.1" },
  { action: "delay", delay: 5000 },
  { action: "clear" },
  { action: "restart" },
];

export const WrongNode = () => {
  return (
    <section className={styles.container}>
      <div className={styles.header}>
        <Text type="h2" className={styles.headerTitle}>
          Have you ever worked on a project and realized you had the wrong node
          version?
        </Text>
        <Text>
          Or golang? Or prettier? Or some other tool? garn manages all of your
          project&apos;s dependencies in encapsulated environments. Compilers,
          code-generators, formatters, test-runners, linters and more.
        </Text>
      </div>
      <div className={styles.terminalContainer}>
        <Terminal title="garn.ts" text={<SampleCode code={CODE} />} />
        <Terminal
          title="terminal"
          text={<TypingText prependedText="$" commands={COMMANDS} />}
          expectedLineCount={6}
        />
      </div>
    </section>
  );
};
