import { Text } from "../text";
import { SampleCode } from "../sampleCode";
import { Terminal } from "../terminal";
import styles from "./styles.module.css";
import { Command, TypingText2 } from "../typingText2";

const CODE = `import * as garn from "https://garn.io/ts/v0.0.14/mod.ts";

export const frontend = garn.javascript.mkNpmProject({
  description: "my frontend project",
  src: ".",
  nodeVersion: "18",
});`;

const TERMINAL = `$ node
node: command not found
$ garn enter frontend
[garn] Entering frontend shell. Type 'exit' to exit.
$ node --version
v18.17.1`;

const TERMINAL2: Command[] = [
  { action: "type", text: "node" },
  { action: "response", text: "node: command not found", delay: 300 },
  { action: "type", text: "garn enter frontend", delay: 1000 },
  {
    action: "response",
    text: "[garn] Entering frontend shell. Type 'exit' to exit.",
    delay: 300,
  },
  { action: "type", text: "node --version", delay: 1000 },
  { action: "response", text: "v18.17.1", delay: 300 },
  { action: "clear", delay: 5000 },
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
          text={<TypingText2 prependedText="$" commands={TERMINAL2} />}
        />
      </div>
    </section>
  );
};
