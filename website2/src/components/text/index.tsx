import { Berlin, MatterSQMono } from "@/utils/fonts";
import { HTMLProps } from "react";
import styles from "./styles.module.css";

interface Props extends HTMLProps<HTMLDivElement> {
  type?: "p" | "h1" | "h2" | "h3" | "proper" | "code";
}

export const Text = ({ type = "p", children, className, ...rest }: Props) => {
  if (type === "p")
    return (
      <p
        className={`${Berlin.className} ${styles.paragraph} ${className}`}
        {...rest}
      >
        {children}
      </p>
    );
  else if (type === "h1")
    return (
      <h1 className={`${styles.header} ${className}`} {...rest}>
        {children}
      </h1>
    );
  else if (type === "h2")
    return (
      <h2 className={`${styles.header} ${className}`} {...rest}>
        {children}
      </h2>
    );
  else if (type === "h3")
    return (
      <h3
        className={`${styles.header} ${styles.header3} ${className}`}
        {...rest}
      >
        {children}
      </h3>
    );
  else if (type === "proper")
    return (
      <span
        className={`${styles.proper} ${className} ${MatterSQMono.className}`}
        {...rest}
      >
        {children}
      </span>
    );
  else if (type === "code")
    return (
      <span
        className={`${styles.code} ${className} ${MatterSQMono.className}`}
        {...rest}
      >
        {children}
      </span>
    );
  else return <div>Text type not found: {type}</div>;
};
