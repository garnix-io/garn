import React from "react";

interface EmailProps {
  address: string;
  domain?: string;
  text?: string;
}

export const Email: React.FC<EmailProps> = ({ address, domain, text }) => {
  const full = `${address}@${domain ?? "garnix.io"}`;
  const linkText = text ? text : full;
  return <a href={`mailto:${full}`}>{linkText}</a>;
};
