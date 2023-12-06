import { Header } from "@/components/header";
import { Bash } from "@/components/homePage/bash";
import { CI } from "@/components/homePage/ci";
import { Hero } from "@/components/homePage/hero";
import { WrongNode } from "@/components/homePage/wrongNode";

const Home = () => {
  return (
    <main>
      <Header />
      <Hero />
      <WrongNode />
      <CI />
      <Bash />
    </main>
  );
};

export default Home;
