"use client";

import { Text } from "@/components/text";
import styles from "./styles.module.css";
import { useState } from "react";
import Link from "next/link";
import { SampleCode } from "@/components/sampleCode";
import { Terminal } from "@/components/terminal";

const CODE = `import * as garn from "https://garn.io/ts/v0.0.20/mod.ts";

export const deno = garn.mkProject({
  description: "garn configuration environment",
  defaultEnvironment: garn.emptyEnvironment.withDevTools([pkgs.deno]),
}, {});`;

const FAQ_ITEMS = [
  {
    question: "How does garn differ from docker, vagrant, etc.?",
    answer: (
      <>
        <div className={styles.paragraph}>
          <Text type="proper">garn</Text> uses nix instead of containers or VMs
          to provide dependencies. These dependencies will all be installed into
          the nix store (in <Text type="code">/nix/store</Text>) and will be
          provided from there in the environments that you create with{" "}
          <Text type="proper">garn</Text>.
        </div>
        <div className={styles.paragraph}>
          That means that <Text type="proper">garn</Text> scripts and checks and
          everything else you do when developing with{" "}
          <Text type="proper">garn</Text> runs natively on your machine. So
          there is no overhead that you would get when using VMs or containers
          on non-linux hosts.
        </div>
        <div className={styles.paragraph}>
          That also means that -- when entering an environment with{" "}
          <Text type="code">garn enter</Text> -- you can use your personal
          editor or other globally installed tools with no additional work.
        </div>
      </>
    ),
  },
  {
    question:
      "How does garn differ from 'npm', 'cabal' and other language-specific tools?",
    answer: (
      <>
        <Text type="proper">garn</Text> is language agnostic. That means you can
        use it in projects that mix multiple languages and have the same
        experience and workflows for all sub-projects. It also means that
        programmers that are unfamiliar with the toolchain of a project can use{" "}
        <Text type="proper">garn</Text> to get started quickly.
      </>
    ),
  },
  {
    question: "What languages does garn support?",
    answer: (
      <>
        <Text type="proper">garn</Text> currently supports <b>Go</b>, <b>Npm</b>{" "}
        and <b>Haskell</b>. If you&apos;d like to see support for other
        languages or toolchains please{" "}
        <Link
          className={styles.link}
          href="https://github.com/garnix-io/garn/issues"
          target="_blank"
        >
          let us know
        </Link>
        .
      </>
    ),
  },
  {
    question:
      "How can I configure LSP to get error messages and auto-completion for garn.ts in my editor?",
    answer: (
      <>
        <div className={styles.paragraph}>
          <Text type="code">garn.ts</Text> files are powered by{" "}
          <Link
            className={styles.link}
            href="https://deno.com/"
            target="_blank"
          >
            Deno
          </Link>
          . A lot of the convenience and power for editing your{" "}
          <Text type="code">garn.ts</Text> files comes from having a working
          Deno LSP. There are two ways of setting up LSP for editing{" "}
          <Text type="code">garn.ts</Text> files: using{" "}
          <Text type="code">garn edit</Text>, which provides a
          properly-configured editor, or using <Text type="proper">garn</Text>{" "}
          to provide <Text type="proper">deno</Text> and setting up your editor
          configuration yourself.
        </div>
        <div className={styles.paragraph}>
          <Text type="code">garn edit</Text>
        </div>
        <div className={styles.paragraph}>
          <Text type="proper">garn</Text> can download and configure{" "}
          <Link className={styles.link} href="https://vscodium.com/">
            vscodium
          </Link>{" "}
          for you — just type <Text type="code">garn edit</Text>. It will spin
          up a <Text type="proper">vscodium</Text> editor that is pre-configured
          for editing <Text type="code">garn.ts</Text> files. It won&apos;t use
          or modify your local vscodium settings, if you have any. You can can
          add it to your <Text type="code">garn.ts</Text> file like this:
        </div>
        <div className={styles.paragraph}>
          <Text type="h3">
            Installing the Deno LSP and configuring your editor
          </Text>
        </div>
        <div className={styles.paragraph}>
          Alternatively you can install deno (including the Deno LSP) with{" "}
          <Text type="proper">garn</Text> itself:
        </div>
        <div className={styles.paragraph}>
          <Terminal
            inverse
            title="garn.ts"
            text={<SampleCode code={CODE} inverse />}
          />
        </div>
        <div className={styles.paragraph}>
          <Text type="proper">garn</Text> enter deno will then drop you in a
          shell where <Text type="proper">deno</Text> is available.
        </div>
        <div className={styles.paragraph}>
          For configuring your editor to use Deno&apos;s LSP refer to{" "}
          <Link
            className={styles.link}
            href="https://docs.deno.com/runtime/manual/getting_started/setup_your_environment"
          >
            Deno&apos;s environment setup documentation
          </Link>
          .
        </div>
      </>
    ),
  },
];

export const FAQ = () => {
  const [openFaq, setOpenFaq] = useState<number>();

  return (
    <section className={styles.container}>
      <div className={styles.content}>
        <Text className={styles.title} type="h2">
          Faq
        </Text>
        {FAQ_ITEMS.map((item, index) => (
          <div
            key={index}
            className={styles.faq}
            onClick={() => setOpenFaq(openFaq === index ? undefined : index)}
          >
            <div className={styles.faqHeader}>
              <Text className={styles.faqTitle} type="h3">
                {item.question}
              </Text>
              <div
                className={`${styles.toggle} ${
                  openFaq === index ? styles.open : ""
                }`}
              >
                +
              </div>
            </div>
            <Text
              type="span"
              className={`${styles.faqAnswer} ${
                openFaq === index ? styles.open : ""
              }`}
            >
              {item.answer}
            </Text>
          </div>
        ))}
      </div>
    </section>
  );
};
