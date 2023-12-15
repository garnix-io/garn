"use client";

import { Document } from "@/components/documentationPage/document";
import { DocumentationHeader } from "@/components/documentationPage/documentationHeader";
import { Link, Sidebar } from "@/components/documentationPage/sidebar";
import styles from "./styles.module.css";
import { useState } from "react";

type Props = {
  source: string;
  links: Link[];
  active: string;
};

export const Container = ({ source, links, active }: Props) => {
  const [isHeaderOpen, setIsHeaderOpen] = useState(false);
  return (
    <div>
      <DocumentationHeader links={links} active={active} />
      <div
        className={`${styles.layout} ${isHeaderOpen ? styles.headerOpen : ""}`}
      >
        <Sidebar links={links} active={active} />
        <Document source={source} />
      </div>
    </div>
  );
};
