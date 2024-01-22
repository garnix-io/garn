"use client";

import { useEffect, useState } from "react";
import Image from "next/image";
import { Text } from "@/components/text";
import { Link as LinkType } from "@/components/documentationPage/sidebar";
import menuIcon from "@/components/icons/menu.svg";
import xIcon from "@/components/icons/x.svg";
import styles from "./styles.module.css";
import Link from "next/link";
import { usePathname } from "next/navigation";

type Props = {
  active: string;
  links: LinkType[];
};

export const DocumentationHeader = ({ active, links }: Props) => {
  const [isOpen, setIsOpen] = useState(false);
  const pathname = usePathname();
  useEffect(() => {
    setIsOpen(false);
  }, [pathname]);
  return (
    <section className={`${styles.container} ${isOpen ? styles.open : ""}`}>
      <div className={styles.content}>
        <Text type="h1">Documentation</Text>
        {isOpen ? (
          <Image
            className={styles.menuToggle}
            src={xIcon}
            alt="close"
            width={20}
            height={40}
            onClick={() => setIsOpen(!isOpen)}
          />
        ) : (
          <Image
            className={styles.menuToggle}
            src={menuIcon}
            alt="menu"
            width={22}
            height={40}
            onClick={() => setIsOpen(!isOpen)}
          />
        )}
      </div>
      <div className={`${styles.menu} ${isOpen ? styles.open : ""}`}>
        {links.map((link) => (
          <Link
            key={link.link}
            href={`/docs/${link.link}`}
            className={`${styles.link} ${
              active === link.link ? styles.active : ""
            }`}
          >
            {link.name}
          </Link>
        ))}
      </div>
    </section>
  );
};
