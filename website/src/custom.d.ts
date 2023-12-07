declare module "asciinema-player" {
  export const create: any;
}

declare module "*.cast" {
  const value: string;
  export = value;
}

declare var plausible:
  | undefined
  | ((eventName: string, args?: { props?: Record<string, string> }) => void);
