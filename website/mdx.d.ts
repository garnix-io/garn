declare module "*.mdx" {
  export const info: {
    title: string;
    url: string;
    date: Date;
    summary: string;
    author: string;
    draft: boolean;
  };
}
