import { Text } from "@/components/text";
import styles from "./styles.module.css";

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
  return (
    <section className={styles.container}>
      <div className={styles.content}>
        <Text className={styles.title} type="h2">
          Faq
        </Text>
        {FAQ_ITEMS.map((item, index) => (
          <div key={index} className={styles.faq}>
            <Text className={styles.faqTitle} type="h3">
              {item.question}
              <div className={styles.toggle}>+</div>
            </Text>
            <Text className={styles.faqAnswer}>{item.answer}</Text>
          </div>
        ))}
      </div>
    </section>
  );
};
