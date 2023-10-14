declare module "asciinema-player" {
  export const create: any;
}

declare module "*.cast" {
  const value: string;
  export = value;
}
