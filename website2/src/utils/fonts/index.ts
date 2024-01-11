import localFont from "next/font/local";
import fonts from "./fonts.module.css";

export const MatterSQMono = {
  className: fonts.MatterSQMono,
};

export const Berlin = localFont({
  src: [
    {
      path: "./BerlinTypeWeb-Regular.woff",
      weight: "400",
    },
  ],
});
