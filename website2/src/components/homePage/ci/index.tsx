import { Text } from "../../text";
import { SampleCode } from "../../sampleCode";
import { Terminal } from "../../terminal";
import styles from "./styles.module.css";
import { Command, TypingText } from "../../typingText";

const CODE = `import * as garn from "https://garn.io/ts/v0.0.14/mod.ts";
import * as nixpkgs from "https://garn.io/ts/v0.0.14/nixpkgs.ts";

export const backend = garn.haskell
  .mkHaskellProject({
    description: "my Haskell backend",
    executable: "server",
    compiler: "ghc94",
    src: ".",
  })
  .withDevTools([nixpkgs.hlint])
  .addCheck("hlint")\`hlint *.hs\`;`;

const COMMANDS: Command[] = [
  { action: "type", text: "garn check" },
  { action: "delay", delay: 300, showCursor: true },
  { action: "response", text: "[...]" },
  { action: "response", text: "check> No hints", delay: 100 },
  { action: "delay", delay: 5000 },
  { action: "clear" },
  { action: "restart" },
];

export const CI = () => {
  return (
    <section className={styles.container}>
      <div className={styles.header}>
        <Text type="h2" className={styles.headerTitle}>
          Have you ever pushed to GitHub over and over again, just to get CI
          working?
        </Text>
        <Text className={styles.headerText}>
          garn allows to declare reproducible checks that you can run locally in
          completely deterministic environments. And if you enable CI (like
          garnix) on your GitHub repo, you get the same exact checks there.
        </Text>
      </div>
      <div className={styles.terminalContainer}>
        <Terminal
          title="garn.ts"
          text={<SampleCode code={CODE} inverse />}
          inverse
        />
        <Terminal
          title="terminal"
          text={<TypingText prependedText="$" commands={COMMANDS} />}
          expectedLineCount={3}
          inverse
        />
      </div>
    </section>
  );
};
