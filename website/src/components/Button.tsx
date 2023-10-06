import React from "react";

export const Button: React.FC = (props) => {
  return <button type="button">{props.children}</button>;
};
