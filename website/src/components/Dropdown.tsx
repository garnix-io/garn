import React, { useState } from "react";
import { Link } from "react-router-dom";

export const DropdownMenu: React.FC<{
  items: { name: string; url: string }[];
  label: string;
}> = ({ label, items }) => {
  const [open, setOpen] = useState(false);

  return (
    <div className="dropdown" onClick={() => setOpen(!open)}>
      {label}
      <ul className={open ? "dropdown show" : "dropdown"}>
        {items.map((item) => (
          <li className="menu-item">
            <Link className="black" to={`docs/${item.url}`}>
              {item.name}
            </Link>
          </li>
        ))}
      </ul>
    </div>
  );
};
