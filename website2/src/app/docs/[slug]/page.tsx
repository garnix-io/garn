import { readFile, readdir } from "fs/promises";
import { serialize } from "next-mdx-remote/serialize";
import { Footer } from "@/components/footer";
import { Header } from "@/components/header";
import { Link, Sidebar } from "@/components/documentationPage/sidebar";
import { DocumentationHeader } from "@/components/documentationPage/documentationHeader";
import { Document } from "@/components/documentationPage/document";
import styles from "./styles.module.css";

const Page = async ({ params: { slug } }: { params: { slug: string } }) => {
  const source = await readFile(`documentation/${slug}.mdx`, "utf-8");
  const links = await fetchLinks();
  return (
    <main>
      <Header />
      <DocumentationHeader links={links} active={slug} />
      <div className={styles.layout}>
        <Sidebar links={links} active={slug} />
        <Document source={source} />
      </div>
      <Footer />
    </main>
  );
};

const fetchLinks = async (): Promise<Link[]> => {
  const filenames = await readdir("documentation");
  const metadata: Link[] = [];
  for (let i = 0; i < filenames.length; i++) {
    const file = await readFile(`documentation/${filenames[i]}`, "utf-8");
    const meta = await serialize<
      null,
      { index: number; name: string; link: string }
    >(file, {
      parseFrontmatter: true,
    });
    const link = filenames[i].split(".mdx")[0];
    metadata.push({
      index: meta.frontmatter.index,
      name: (meta.frontmatter.name as string) || link.split("_").join(" "),
      link,
    });
  }
  return metadata.sort((a, b) => a.index - b.index);
};

export const generateStaticParams = async () => {
  const res = await readdir("documentation");
  return res.map((document) => ({
    slug: document.split(".mdx")[0],
  }));
};

export default Page;
