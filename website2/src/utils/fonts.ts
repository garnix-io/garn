import localFont from "next/font/local";

export const MatterSQMono = localFont({
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

export const Berlin = localFont({
  src: [
    {
      path: "../../public/fonts/BerlinTypeWeb-Regular.woff",
      weight: "400",
    },
  ],
});
