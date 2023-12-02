import type { Metadata } from "next";
import localFont from "next/font/local";
import "./globals.css";
import "@/utils/colors.css";

const MatterSQMono = localFont({
  src: [
    {
      path: "../../public/fonts/MatterSQMonoTRIAL-Light.woff",
      weight: "300",
    },
    {
      path: "../../public/fonts/MatterSQMonoTRIAL-Regular.woff",
      weight: "400",
    },
  ],
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
