import { Text } from "../text";
import { SampleCode } from "../sampleCode";
import { Terminal } from "../terminal";
import styles from "./styles.module.css";

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
        <Terminal title="terminal" text={TERMINAL} />
      </div>
    </section>
  );
};
