import { MDXRemote } from "next-mdx-remote/rsc";
import { Terminal } from "@/components/terminal";
import { Text } from "@/components/text";
import styles from "./styles.module.css";
import { SampleCode } from "@/components/sampleCode";
import { Berlin } from "@/utils/fonts";
import { ToolTip } from "@/components/tooltip/";
import Link from "next/link";
import { Share } from "@/components/icons/share";
import { DocumentHeader } from "../documentHeader";

type Props = {
  source: string;
};

export const Document = ({ source }: Props) => {
  return (
    <section className={styles.container}>
      <MDXRemote
        source={source}
        components={{
          ToolTip: (props) => (
            <ToolTip
              {...props}
              className={`${props.className} ${styles.tooltip}`}
            >
              {props.children}
            </ToolTip>
          ),
          Text,
          p: (props) => (
            <Text
              type="p"
              {...props}
              className={`${props.className} ${styles.p}`}
            >
              {props.children}
            </Text>
          ),
          h1: (props) => <DocumentHeader type="h1" {...props} />,
          h2: (props) => (
            <DocumentHeader
              type="h2"
              {...props}
              className={`${props.className} ${styles.h2}`}
            />
          ),
          h3: (props) => (
            <DocumentHeader
              type="h3"
              {...props}
              className={`${props.className} ${styles.h3}`}
            />
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
