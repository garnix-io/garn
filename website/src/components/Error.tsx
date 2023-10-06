import React from "react";

interface ServerErrorProps {
  message?: string;
}

export const ServerError: React.FC<ServerErrorProps> = (props) => {
  return (
    <div>
      <b>Uh oh!</b>
      <span>{props.message || "Something went wrong"}</span>
    </div>
  );
};
