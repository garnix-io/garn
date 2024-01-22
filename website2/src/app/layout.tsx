import type { Metadata } from "next";
import "./globals.css";
import "@/utils/colors.css";
import "@/utils/sizes.css";
import { MatterSQMono } from "@/utils/fonts";

export const metadata: Metadata = {
  title: "garn",
  description: "A build tool and environment manager",
};

const RootLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <html lang="en">
      <head>
        <script
          defer
          data-domain="garn.io"
          src="https://plausible.io/js/script.outbound-links.tagged-events.js"
        ></script>
      </head>
      <body className={MatterSQMono.className}>{children}</body>
    </html>
  );
};

export default RootLayout;
