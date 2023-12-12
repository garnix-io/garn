"use client";

import { Tooltip as InternalTooltip } from "@nextui-org/react";
import styles from "./styles.module.css";
import { PropsWithChildren, useState } from "react";
import { useWindowSize } from "usehooks-ts";
import { Text } from "@/components/text";

type Props = PropsWithChildren & {
  className: string;
};

export const ToolTip = ({ children, className, ...rest }: Props) => {
  const [isOpen, setIsOpen] = useState(false);
  const { width } = useWindowSize();
  return (
    <InternalTooltip
      content={<Text className={styles.content}>{children}</Text>}
      placement={width > 768 ? "right" : "top"}
      isOpen={isOpen}
      {...rest}
    >
      <div
        className={`${className} ${styles.button}`}
        onClick={() => setIsOpen(!isOpen)}
        onMouseEnter={() => setIsOpen(true)}
        onMouseLeave={() => setIsOpen(false)}
      >
        ?
      </div>
    </InternalTooltip>
  );
};
