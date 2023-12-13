import { z } from "zod";

export const checkPropsRuntime = <T>(
  p: z.ZodType<T>,
  fn: (props: T) => React.ReactNode
): React.FC<T> => {
  return (props) => fn(p.parse(props));
};
