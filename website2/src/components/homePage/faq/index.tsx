"use client";

import { Text } from "@/components/text";
import styles from "./styles.module.css";
import { useState } from "react";

const FAQ_ITEMS = [
  {
    question: "How does garn differ from docker, vagrant, etc.?",
    answer: (
      <span>
        <p className={styles.paragraph}>
          <Text type="proper">garn</Text> uses nix instead of containers or VMs
          to provide dependencies. These dependencies will all be installed into
          the nix store (in <Text type="code">/nix/store</Text>) and will be
          provided from there in the environments that you create with{" "}
          <Text type="proper">garn</Text>.
        </p>
        <p className={styles.paragraph}>
          That means that <Text type="proper">garn</Text> scripts and checks and
          everything else you do when developing with{" "}
          <Text type="proper">garn</Text> runs natively on your machine. So
          there is no overhead that you would get when using VMs or containers
          on non-linux hosts.
        </p>
        <p className={styles.paragraph}>
          That also means that -- when entering an environment with{" "}
          <Text type="code">garn enter</Text> -- you can use your personal
          editor or other globally installed tools with no additional work.
        </p>
      </span>
    ),
  },
  {
    question:
      "How does garn differ from 'npm', 'cabal' and other language-specific tools?",
    answer: (
      <span>
        <Text type="proper">garn</Text> is language agnostic. That means you can
        use it in projects that mix multiple languages and have the same
        experience and workflows for all sub-projects. It also means that
        programmers that are unfamiliar with the toolchain of a project can use{" "}
        <Text type="proper">garn</Text> to get started quickly.
      </span>
    ),
  },
  {
    question: (
      <span>
        What languages does <Text type="proper">garn</Text> support?
      </span>
    ),
    answer: "Lorem ipsum dolor sit amet",
  },
  {
    question:
      "How can I configure LSP to get error messages and auto-completion for garn.ts in my editor?",
    answer: "Lorem ipsum dolor sit amet",
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
