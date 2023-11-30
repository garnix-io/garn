import Link from "next/link";

export const Header = () => {
  return (
    <header>
      <section>
        <Link href="/">garn</Link>
        <Link href="/documentation">documentation</Link>
      </section>
      <section className="desktop">
        <Link href="https://discord.gg/XtDrPsqpVx" target="_blank">
          discord
        </Link>
        <Link href="https://github.com/garnix-io/garn" target="_blank">
          github
        </Link>
      </section>
      <section className="mobile">
        <Link href="/">garn</Link>
        <Link href="/documentation">documentation →</Link>
      </section>
    </header>
  );
};

/*
header {
  height: 72px;
  display: flex;
  justify-content: space-between;
  background-color: black;
  text-transform: uppercase;
  padding: 0 32px;
  align-items: center;
}

header section {
  display: flex;
  gap: 32px;
}

header section.mobile {
  display: none;
}

header section a {
  color: #F2F2F2;
  font-family: MatterSQMono, monospace;
  font-size: 24px;
  line-height: 40px;
}

header section a:hover {
  text-decoration: underline;
}

@media only screen and (max-width: 768px) {
  header {
    height: 48px;
    padding: 0 16px;
  }

  header section.desktop {
    display: none;
  }

  header section.mobile {
    display: flex;
    width: 100%;
    justify-content: space-between;
  }

  header section a {
    font-size: 16px;
    line-height: 28px;
  }
}
*/
