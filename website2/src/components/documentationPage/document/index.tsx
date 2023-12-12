import { MDXRemote } from "next-mdx-remote/rsc";
import { Terminal } from "@/components/terminal";
import { Text } from "@/components/text";
import { Hovernote } from "@/components/documentationPage/hovernote";
import styles from "./styles.module.css";
import { SampleCode } from "@/components/sampleCode";
import { Berlin } from "@/utils/fonts";

type Props = {
  source: string;
};

export const Document = ({ source }: Props) => {
  return (
    <section className={styles.container}>
      <MDXRemote
        source={source}
        components={{
          Terminal: (props) => (
            <Terminal
              {...props}
              className={`${props.className} ${styles.terminal}`}
            >
              {props.children}
            </Terminal>
          ),
          Text,
          Hovernote,
          p: (props) => (
            <Text
              type="p"
              {...props}
              className={`${props.className} ${styles.p}`}
            >
              {props.children}
            </Text>
          ),
          h1: (props) => (
            <Text type="h1" {...props}>
              {props.children}
            </Text>
          ),
          h2: (props) => (
            <Text
              type="h2"
              {...props}
              className={`${props.className} ${styles.h2}`}
            >
              {props.children}
            </Text>
          ),
          h3: (props) => (
            <Text
              type="h3"
              {...props}
              className={`${props.className} ${styles.h3}`}
            >
              {props.children}
            </Text>
          ),
          ul: (props) => (
            <ul
              {...props}
              className={`${props.className} ${styles.ul} ${Berlin.className}`}
            >
              {props.children}
            </ul>
          ),
          ol: (props) => (
            <ol {...props} className={`${props.className} ${styles.ol}`}>
              {props.children}
            </ol>
          ),
          li: (props) => (
            <li
              {...props}
              className={`${props.className} ${styles.li} ${Berlin.className}`}
            >
              {props.children}
            </li>
          ),
          code: (props) =>
            props.className ? (
              <Terminal
                {...props}
                className={`${props.className} ${styles.terminal}`}
                text={
                  <SampleCode
                    code={props.children?.toString() || ""}
                    inverse
                    language={
                      props.className.includes("language-")
                        ? props.className.split("language-")[1]
                        : "javascript"
                    }
                  />
                }
              />
            ) : (
              <Text type="code" className={`${props.className} ${styles.code}`}>
                {props.children}
              </Text>
            ),
        }}
      />
    </section>
  );
};
