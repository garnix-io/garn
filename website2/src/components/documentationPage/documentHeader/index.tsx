import { Text } from "@/components/text";
import { PropsWithChildren, ReactNode } from "react";
import styles from "./styles.module.css";
import { Share } from "@/components/icons/share";
import Link from "next/link";

type Props = PropsWithChildren & {
  type: "h1" | "h2" | "h3";
  className?: string;
};

export const DocumentHeader = ({ type, children, ...rest }: Props) => {
  const id = getId(children);
  return (
    <Text
      id={id}
      type={type}
      {...rest}
      className={`${rest.className} ${styles.container}`}
    >
      <Link href={`#${id}`} className={styles.link}>
        <span>{children}</span>
        <Share className={styles.share} />
      </Link>
    </Text>
  );
};

const getId = (children: ReactNode): string => {
children.toLowerCase().replace(/[^a-zA-Z0-9]+/g, "-")
};
