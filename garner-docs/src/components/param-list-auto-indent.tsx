import { React } from "../../deps.client.ts";

const IndentCtx = React.createContext(0);
const INDENT_TO_ADD = 4;

/**
 * ParameterListAutoIndent will concatenate items in `list` using `joiner`
 * (default comma) but automatically add newlines and indent when the width is
 * too large to comfortably render on a single line
 */
export default function ParameterListAutoIndent(
  props: {
    list: Array<React.ReactNode>;
    joiner?: React.ReactNode;
    opener?: React.ReactNode;
    closer?: React.ReactNode;
  },
) {
  const ref = React.useRef<HTMLSpanElement>(null);
  const [shouldIndent, setShouldIndent] = React.useState(false);
  const indentLvl = React.useContext(IndentCtx);

  React.useEffect(() => {
    if (!shouldIndent) {
      setShouldIndent(
        props.list.length > 0 &&
          (ref.current?.getBoundingClientRect().width ?? 0) > 500,
      );
    }
  }, [props.list]);

  return (
    // @ts-expect-error - this seems to be a react 18 bug with deno, typescript
    // does not like the return type of IndentCtx.Provider even though it works
    // fine at runtime.
    <IndentCtx.Provider value={indentLvl + shouldIndent ? INDENT_TO_ADD : 0}>
      <span ref={ref} style={{ whiteSpace: "pre" }}>
        {props.opener}
        {props.list.reduce((acc: Array<React.ReactNode>, item) => [
          ...acc,
          ...(acc.length > 0 ? [props.joiner || ", "] : []),
          shouldIndent && `\n${" ".repeat(indentLvl + INDENT_TO_ADD)}`,
          item,
        ], [])}
        {shouldIndent && `\n${" ".repeat(indentLvl)}`}
        {props.closer}
      </span>
    </IndentCtx.Provider>
  );
}
