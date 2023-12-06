import { Text } from "../../text";
import { SampleCode } from "../../sampleCode";
import { Terminal } from "../../terminal";
import styles from "./styles.module.css";
import { Command, TypingText } from "../../typingText";

const CODE = `export const backend = garn.go.mkGoProject({
  description: "Go backend",
  src: ".",
  goVersion: "1.20",
})
  .withDevTools([pkgs.protobuf, pkgs.protoc_gen_go])
  .addExecutable("codegen", "protoc --go_out=out protobufs/*.proto");`;

const COMMANDS: Command[] = [
  { action: "type", text: "protoc protobufs/*.proto" },
  { action: "delay", delay: 300, showCursor: true },
  { action: "response", text: "protoc: command not found\n" },
  { action: "delay", delay: 1000 },
  { action: "type", text: "garn run backend.codegen" },
  { action: "delay", delay: 5000 },
  { action: "clear" },
  { action: "restart" },
];

export const Bash = () => {
  return (
    <section className={styles.container}>
      <div className={styles.textContainer}>
        <Text type="h2">
          Have you ever written a bash script that worked totally fine on your
          machine, but crashed on someone else&apos;s?
          <br />
          <br />
          Well, it was probably their fault for not having the right tools
          installed...
        </Text>
      </div>
      <div className={styles.terminalContainer}>
        <Terminal title="garn.ts" text={<SampleCode code={CODE} />} />
        <Terminal
          title="terminal"
          text={<TypingText prependedText="$" commands={COMMANDS} />}
          expectedLineCount={4}
        />
      </div>
      <Text className={styles.textContainer}>
        garn allows you to add deterministic scripts to your projects that run
        the same everywhere. Use them to run dev servers, bundle Javascript,
        format source code, run code generators and more.
      </Text>
    </section>
  );
};
