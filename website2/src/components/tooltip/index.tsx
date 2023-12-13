"use client";

import { Tooltip as InternalTooltip } from "@nextui-org/react";
import { useState } from "react";
import { useWindowSize } from "usehooks-ts";
import { z } from "zod";
import { Text } from "@/components/text";
import styles from "./styles.module.css";
import { checkPropsRuntime } from "@/utils/checkPropsRuntime";

const PropSchema = z
  .object({
    className: z.string().optional(),
    children: z.custom(),
  })
  .strict();

export const ToolTip = checkPropsRuntime(
  PropSchema,
  ({ children, className }: z.infer<typeof PropSchema>) => {
    const [isOpen, setIsOpen] = useState(false);
    const { width } = useWindowSize();
    return (
      <InternalTooltip
        content={<Text className={styles.content}>{children}</Text>}
        placement={width > 768 ? "right" : "top"}
        isOpen={isOpen}
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
  }
);
