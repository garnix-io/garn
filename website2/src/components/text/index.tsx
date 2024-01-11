import z from "zod";
import { Berlin, MatterSQMono } from "@/utils/fonts";
import styles from "./styles.module.css";
import { withPropCheck } from "@/utils/withPropCheck";

const PropSchema = z.object({
  id: z.string().optional(),
  type: z.enum(["p", "h1", "h2", "h3", "proper", "code", "span"]).optional(),
  className: z.string().optional(),
  children: z.custom(),
});

export const Text = withPropCheck(
  PropSchema,
  ({ type = "p", children, className, ...rest }) => {
    if (type === "p")
      return (
        <p
          {...rest}
          className={`${Berlin.className} ${styles.paragraph} ${className}`}
        >
          {children}
        </p>
      );
    else if (type === "h1")
      return (
        <h1 {...rest} className={`${styles.header} ${className}`}>
          {children}
        </h1>
      );
    else if (type === "h2")
      return (
        <h2 {...rest} className={`${styles.header} ${className}`}>
          {children}
        </h2>
      );
    else if (type === "h3")
      return (
        <h3
          {...rest}
          className={`${styles.header} ${styles.header3} ${className}`}
        >
          {children}
        </h3>
      );
    else if (type === "proper")
      return (
        <span
          {...rest}
          className={`${styles.proper} ${className} ${MatterSQMono.className}`}
        >
          {children}
        </span>
      );
    else if (type === "code")
      return (
        <span
          {...rest}
          className={`${styles.code} ${className} ${MatterSQMono.className}`}
        >
          {children}
        </span>
      );
    else if (type === "span")
      return (
        <span
          {...rest}
          className={`${Berlin.className} ${styles.paragraph} ${className}`}
        >
          {children}
        </span>
      );
    else return <div>Text type not found: {type}</div>;
  }
);
