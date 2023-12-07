"use client";

import { Text } from "@/components/text";
import styles from "./styles.module.css";
import { useState } from "react";

const FAQ_ITEMS = [
  {
    question: "Lorem ipsum dolor sit amet",
    answer: "Lorem ipsum dolor sit amet",
  },
  {
    question: "Lorem ipsum dolor sit amet",
    answer: "Lorem ipsum dolor sit amet",
  },
  {
    question: "Lorem ipsum dolor sit amet",
    answer: "Lorem ipsum dolor sit amet",
  },
  {
    question: "Lorem ipsum dolor sit amet",
    answer: "Lorem ipsum dolor sit amet",
  },
  {
    question: "Lorem ipsum dolor sit amet",
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
