import { Text } from "@/components/text";
import { ReactNode } from "react";
import styles from "./styles.module.css";
import { Share } from "@/components/icons/share";
import Link from "next/link";
import { z } from "zod";
import { withPropCheck } from "@/utils/withPropCheck";

const PropSchema = z.object({
  type: z.enum(["h1", "h2", "h3"]),
  className: z.string().optional(),
  children: z.custom(),
});

export const DocumentHeader = withPropCheck(
  PropSchema,
  ({ type, className, children }) => {
    const id = getId(children);
    return (
      <Text id={id} type={type} className={`${className} ${styles.container}`}>
        <Link href={`#${id}`} className={styles.link}>
          <span>{children}</span>
          <Share className={styles.share} />
        </Link>
      </Text>
    );
  }
);

const getId = (children: ReactNode): string => {
  return children?.toString().toLowerCase().split(" ").join("-") || "";
};
