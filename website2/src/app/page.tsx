import { Header } from "@/components/header";
import { CI } from "@/components/homePage/ci";
import { FAQ } from "@/components/homePage/faq";
import { Hero } from "@/components/homePage/hero";
import { WrongNode } from "@/components/homePage/wrongNode";

const Home = () => {
  return (
    <main>
      <Header />
      <Hero />
      <WrongNode />
      <CI />
      <FAQ />
    </main>
  );
};

export default Home;
