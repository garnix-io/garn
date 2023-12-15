import { Text } from "@/components/text";
import { ReactNode, isValidElement } from "react";
import styles from "./styles.module.css";
import { Share } from "@/components/icons/share";
import Link from "next/link";
import { withPropCheck } from "@/utils/withPropCheck";
import { z } from "zod";

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
  if (!children) return "undefined";
  if (["string", "number"].includes(typeof children))
    return `${children}`
      .trim()
      .toLowerCase()
      .replace(/[^a-zA-Z0-9]+/g, "-");
  else if (children instanceof Array) {
    return children.map(getId).join("-");
  } else if (isValidElement(children)) {
    return getId(children.props.children);
  }
  return children.toString();
};
