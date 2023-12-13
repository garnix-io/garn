import { ZodRawShape, ZodType, z } from "zod";

export const withPropCheck = <T>(
  p: z.ZodObject<ZodRawShape, "strict", ZodType<T>>,
  fn: (props: T) => React.ReactNode
): React.FC<T> => {
  return (props) => fn(p.strict().parse(props) as T);
};
