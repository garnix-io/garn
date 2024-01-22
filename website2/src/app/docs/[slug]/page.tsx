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
        <Document className={styles.document} source={source} />
      </div>
      <Footer />
    </main>
  );
};

const getDocs = async (): Promise<
  Array<{ fileName: string; slug: string }>
> => {
  const fileNames = await readdir("documentation");
  return fileNames.map((fileName) => ({
    fileName,
    slug: fileName.split(".mdx")[0],
  }));
};

const fetchLinks = async (): Promise<Link[]> => {
  const docs = await getDocs();
  const metadata: Link[] = [];
  for (let i = 0; i < docs.length; i++) {
    const file = await readFile(`documentation/${docs[i].fileName}`, "utf-8");
    const meta = await serialize<
      null,
      { index: number; name: string; link: string }
    >(file, {
      parseFrontmatter: true,
    });
    metadata.push({
      index: meta.frontmatter.index,
      name:
        (meta.frontmatter.name as string) || docs[i].slug.replaceAll("_", " "),
      link: docs[i].slug,
    });
  }
  return metadata.sort((a, b) => a.index - b.index);
};

export const generateStaticParams = () => {
  return getDocs();
};

export default Page;
