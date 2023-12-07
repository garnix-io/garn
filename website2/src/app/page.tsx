import { Header } from "@/components/header";
import { Bash } from "@/components/homePage/bash";
import { CI } from "@/components/homePage/ci";
import { FAQ } from "@/components/homePage/faq";
import { Hero } from "@/components/homePage/hero";
import { Social } from "@/components/homePage/social";
import { WrongNode } from "@/components/homePage/wrongNode";

const Home = () => {
  return (
    <main>
      <Header />
      <Hero />
      <WrongNode />
      <CI />
      <Bash />
      <FAQ />
      <Social />
    </main>
  );
};

export default Home;
