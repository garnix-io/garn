export const dbg = <A,>(a: A): A => {
  console.error("DBG =>", a);
  return a;
};
