import { Header } from "@/components/header";
import { Bash } from "@/components/homePage/bash";
import { Hero } from "@/components/homePage/hero";
import { WrongNode } from "@/components/homePage/wrongNode";

const Home = () => {
  return (
    <main>
      <Header />
      <Hero />
      <WrongNode />
      <Bash />
    </main>
  );
};

export default Home;
