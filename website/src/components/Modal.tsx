import React from "react";

interface ModalProps {
  show: boolean;
  close: () => void;
}

export const Modal: React.FC<ModalProps> = ({ show, close, children }) => {
  const showClass = show ? "modal display-block" : "modal display-none";

  return (
    <div className={showClass}>
      <section className="modal-main">{children}</section>
    </div>
  );
};
