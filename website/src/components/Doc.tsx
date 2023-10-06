import React, { useEffect } from "react";
import {
  BrowserRouter,
  Routes,
  useParams,
  Link,
  Route,
} from "react-router-dom";

interface DocProps {
  title: string;
  is_index?: boolean;
  url?: string;
}

export const Doc: React.FC<DocProps> = ({ title, url, is_index, children }) => {
  const param = useParams();
  const my_url = url ?? title;

  if (is_index) {
    return (
      <div>
        <Link to={my_url}>{title}</Link>
      </div>
    );
  }
  if (my_url === param.docItem) {
    return (
      <div>
        <h2>{title}</h2>
        <div>{children}</div>
      </div>
    );
  } else {
    return <div />;
  }
};
