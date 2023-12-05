import { Header } from "@/components/header";
import { Hero } from "@/components/homePage/hero";
import { WrongNode } from "@/components/homePage/wrongNode";

const Home = () => {
  return (
    <main>
      <Header />
      <Hero />
      <WrongNode />
    </main>
  );
};

export default Home;
