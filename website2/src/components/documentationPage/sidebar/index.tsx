import Link from "next/link";
import styles from "./styles.module.css";

export type Link = {
  index: number;
  name: string;
  link: string;
};

type Props = {
  links: Link[];
  active: string;
};

export const Sidebar = ({ links, active }: Props) => {
  return (
    <section className={styles.container}>
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
      <Link
        className={styles.link}
        href="https://doc.deno.land/https://garn.io/ts/v0.0.20/mod.ts"
        target="_blank"
      >
        Typescript API
      </Link>
    </section>
  );
};
