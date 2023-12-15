import { Text } from "@/components/text";
import { PropsWithChildren, ReactNode, isValidElement } from "react";
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
