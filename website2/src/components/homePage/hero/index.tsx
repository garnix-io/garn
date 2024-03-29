import styles from "./styles.module.css";
import { Terminal } from "../../terminal";
import { SampleCode } from "../../sampleCode";
import { Command, TypingText } from "../../typingText";
import { HeaderWithActions } from "@/components/headerWithActions";

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

const CODE = `import * as garn from "https://garn.io/ts/v0.0.20/mod.ts";
import * as pkgs from "https://garn.io/ts/v0.0.20/nixpkgs.ts";

export const backend = garn.go.mkGoProject({
  description: "A Go server",
  src: ".",
  goVersion: "1.21",
})
  .withDevTools([pkgs.ripgrep, pkgs.air])
  .addCheck("no-todos-allowed", "! rg -i todo .")
  .addExecutable("dev-server", "air main.go");`;

export const Hero = () => {
  return (
    <section className={styles.container}>
      <div className={styles.content}>
        <HeaderWithActions
          title="Declaratively configure your projects, get reproducible results"
          links={[
            {
              text: "Get started",
              href: "/docs/getting_started",
            },
            {
              text: "READ MORE",
              href: "/docs/tutorial",
            },
          ]}
        />
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
