import type { Metadata } from "next";
import localFont from "next/font/local";
import "./globals.css";
import "@/utils/colors.css";

const MatterSQMono = localFont({
  src: "../../public/fonts/MatterSQMonoTRIAL-Light.woff",
});

export const metadata: Metadata = {
  title: "garn",
  description: "A build tool and environment manager",
};

const RootLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <html lang="en">
      <body className={MatterSQMono.className}>{children}</body>
    </html>
  );
};

export default RootLayout;
