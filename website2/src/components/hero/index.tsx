import { Prism as SyntaxHighlighter } from "react-syntax-highlighter";
import Link from "next/link";
import styles from "./styles.module.css";
import { TypingText } from "../typingText";
import { oneLight } from "react-syntax-highlighter/dist/esm/styles/prism";
import { Terminal } from "../terminal";

const TERMINAL_EXAMPLES = [
  "check backend.no-todos-allowed",
  "run backend.dev-server",
  "enter backend",
  "build backend",
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
          <div className={styles.header}>
            Declaratively configure your projects, get reproducible results
          </div>
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
        <Terminal
          title="garn.ts"
          text={
            <SyntaxHighlighter
              language="javascript"
              style={oneLight}
              customStyle={{
                background: "transparent",
                padding: 0,
                paddingBottom: 8,
                margin: 0,
              }}
              codeTagProps={{ style: { background: "transparent" } }}
            >
              {CODE}
            </SyntaxHighlighter>
          }
        />
        <Terminal
          title="terminal"
          text={<TypingText prependedText="$ garn" texts={TERMINAL_EXAMPLES} />}
        />
      </div>
    </section>
  );
};
