import React, { useState, useEffect, ReactNode } from "react";
import {
  useFloating,
  useInteractions,
  useHover,
  useFocus,
  useDismiss,
  useRole,
  flip,
  shift,
} from "@floating-ui/react-dom-interactions";

interface TooltipProps {
  item: React.FC
}

export const Tooltip: React.FC<{item: ReactNode}> = ({ item, children }) => {
  const [open, setOpen] = useState(false);
  const { x, y, reference, floating, refs, strategy, context } = useFloating({
    placement: "top-start",
    middleware: [flip(), shift()],
    open,
    onOpenChange: setOpen,
  });
  const { getReferenceProps, getFloatingProps } = useInteractions([
    useHover(context),
    useFocus(context),
    useRole(context, { role: "tooltip" }),
    useDismiss(context),
  ]);

  return (
    <>
      <span
        ref={reference}
        onMouseEnter={() => setOpen(true)}
        onMouseLeave={() => setOpen(false)}
        onClick={() => setOpen(!open)}
        className="tooltip"
      >
      {item}</span>
      {open && (
        <div
          ref={floating}
          className="tooltip-content"
          style={{
            position: strategy,
            top: y ?? 0,
            left: x ?? 0,
            visibility: open ? "visible" : "hidden",
          }}
        >
          {children}
        </div>
      )}
    </>
  );
};

export const Hovernote: React.FC = ({ children }) => {
  const [open, setOpen] = useState(false);
  const { x, y, reference, floating, refs, strategy, context } = useFloating({
    placement: "top-start",
    middleware: [flip(), shift()],
    open,
    onOpenChange: setOpen,
  });
  const { getReferenceProps, getFloatingProps } = useInteractions([
    useHover(context),
    useFocus(context),
    useRole(context, { role: "tooltip" }),
    useDismiss(context),
  ]);

  return (
    <>
      <span
        ref={reference}
        aria-describedby="footnote-label"
        onMouseEnter={() => setOpen(true)}
        onMouseLeave={() => setOpen(false)}
        onClick={() => setOpen(!open)}
        className="hovernote"
      />
      {open && (
        <div
          ref={floating}
          className="hovernote-content"
          style={{
            position: strategy,
            top: y ?? 0,
            left: x ?? 0,
            visibility: open ? "visible" : "hidden",
          }}
        >
          {children}
        </div>
      )}
    </>
  );
};
