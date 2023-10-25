import React from "react";
import { Box } from "reakit/Box";

export type TypographyVariant = "h1" | "h2" | "body";

interface TypographyProps {
  variant: TypographyVariant;
  className?: string;
  children: React.ReactNode;
}

export const Typography: React.FC<TypographyProps> = ({
  variant,
  children,
  ...props
}) => {
  return (
    <Box as={typographyElement(variant)} {...props}>
      {children}
    </Box>
  );
};

const typographyElement = (variant: TypographyVariant) => {
  switch (variant) {
    case "body":
      return "p";
    default:
      return variant;
  }
};
interface BlockQuoteProps {
  author: string;
  source: string;
}

export const BlockQuote: React.FC<BlockQuoteProps> = ({
  author,
  source,
  children,
}) => {
  return (
    <figure className="quote-figure">
      <blockquote>{children}</blockquote>
      <figcaption>
        &mdash;{author}, <cite>{source}</cite>{" "}
      </figcaption>
    </figure>
  );
};

export const SmallCaps: React.FC = ({ children }) => {
  return <span className="small-caps">{children}</span>;
};

export const Nix = () => {
  return <SmallCaps>Nix</SmallCaps>;
};

export const CI = () => {
  return <SmallCaps>CI</SmallCaps>;
};

export const Garnix = () => {
  return <span className="garnix-name">garnix</span>;
};

export const Garn = () => {
  return <span className="garnix-name">garn</span>;
};
