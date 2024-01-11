import localFont from "next/font/local";

export const MatterSQMono = localFont({
  src: [
    {
      path: "./MatterSQMono-Light.woff",
      weight: "300",
    },
    {
      path: "./MatterSQMono-Regular.woff",
      weight: "400",
    },
  ],
});

export const Berlin = localFont({
  src: [
    {
      path: "./BerlinTypeWeb-Regular.woff",
      weight: "400",
    },
  ],
});
