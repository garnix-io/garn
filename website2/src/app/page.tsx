import { Header } from "@/components/header";
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
    </main>
  );
};

export default Home;
