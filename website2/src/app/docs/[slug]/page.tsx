import { readFile, readdir } from "fs/promises";
import { DocumentationHeader } from "@/components/documentationPage/documentationHeader";
import { Footer } from "@/components/footer";
import { Header } from "@/components/header";
import { Document } from "@/components/documentationPage/document";

const Page = async ({ params: { slug } }: { params: { slug: string } }) => {
  const source = await readFile(`documentation/${slug}.mdx`, "utf-8");
  return (
    <main>
      <Header />
      <DocumentationHeader />
      <Document source={source} />
      <Footer />
    </main>
  );
};

export const generateStaticParams = async () => {
  const res = await readdir("documentation");
  return res.map((document) => ({
    slug: document.split(".mdx")[0],
  }));
};

export default Page;
