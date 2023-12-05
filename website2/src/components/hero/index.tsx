import Link from "next/link";
import styles from "./styles.module.css";
import { Terminal } from "../terminal";
import { SampleCode } from "../sampleCode";
import { Text } from "../text";
import { Command, TypingText } from "../typingText";

const COMMANDS: Command[] = [
  { action: "type", text: "check backend.no-todos-allowed" },
  { action: "delete", delay: 2500, showCursor: true },
  { action: "type", text: "run backend.dev-server" },
  { action: "delete", delay: 2500, showCursor: true },
  { action: "type", text: "enter backend" },
  { action: "delete", delay: 2500, showCursor: true },
  { action: "type", text: "build backend" },
  { action: "delete", delay: 2500, showCursor: true },
  { action: "restart" },
];

const CODE = `import * as garn from "https://garn.io/ts/v0.0.14/mod.ts";
import * as pkgs from "https://garn.io/ts/v0.0.14/nixpkgs.ts";

export const backend = garn.go.mkGoProject({   
  description: "A Go server",  
  src: ".",  
  goVersion: "1.20",
})  
  .withDevTools([pkgs.ripgrep, pkgs.air])  
  .addCheck("no-todos-allowed")\`! rg -i todo .\`  
  .addExecutable("dev-server")\`air main.go\`;
`;

export const Hero = () => {
  return (
    <section className={styles.container}>
      <div className={styles.content}>
        <div className={styles.section}>
          <Text type="h1" className={styles.header}>
            Declaratively configure your projects, get reproducible results
          </Text>
          <div className={styles.actions}>
            <Link className={styles.action} href="/documentation/get-started">
              <div className={styles.text}>Get started</div>
              <div className={styles.arrow}>&rarr;</div>
            </Link>
            <Link className={styles.action} href="/documentation/read-more">
              <div className={styles.text}>READ MORE</div>
              <div className={styles.arrow}>&rarr;</div>
            </Link>
          </div>
        </div>
        <div className={styles.mobileDivider} />
        <Terminal title="garn.ts" text={<SampleCode code={CODE} />} />
        <Terminal
          title="terminal"
          text={<TypingText prependedText="$ garn" commands={COMMANDS} />}
        />
      </div>
    </section>
  );
};
