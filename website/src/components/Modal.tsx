import React from "react";

interface ModalProps {
  show: boolean;
  onClose: () => void;
}

export const Modal: React.FC<ModalProps> = ({ show, onClose, children }) => {
  const showClass = show ? "modal display-block" : "modal display-none";

  return (
    <div className={showClass}>
      <section className="modal-main">{children}</section>
    </div>
  );
};
