import { DocumentationHeader } from "@/components/documentationPage/documentationHeader";
import { Footer } from "@/components/footer";
import { Header } from "@/components/header";

const Page = () => {
  return (
    <main>
      <Header />
      <DocumentationHeader />
      <Footer />
    </main>
  );
};

export default Page;
