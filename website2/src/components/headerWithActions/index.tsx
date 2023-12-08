import { Text } from "@/components/text";
import styles from "./styles.module.css";
import Link from "next/link";

type Props = {
  title: string;
  links: {
    text: string;
    href: string;
  }[];
  className?: string;
};

export const HeaderWithActions = ({ title, links, className = "" }: Props) => {
  return (
    <div className={`${styles.section} ${className}`}>
      <Text type="h1" className={styles.header}>
        {title}
      </Text>
      <div className={styles.actions}>
        {links.map((link, index) => (
          <Link
            key={index}
            className={styles.action}
            href={link.href}
            target={link.href.includes("http") ? "_blank" : ""}
          >
            <div className={styles.text}>{link.text}</div>
            <div className={styles.arrow}>&rarr;</div>
          </Link>
        ))}
      </div>
    </div>
  );
};
