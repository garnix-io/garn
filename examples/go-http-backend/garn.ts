import * as garn from "http://localhost:8777/mod.ts";

export const server: garn.Project = garn.go.mkGoProject({
  description: "example backend server in go",
  src: ".",
  goVersion: "1.20",
});

export const dev = server.shell`go run ./main.go`;

export const migrate: garn.Executable = server.shell`go run ./scripts/migrate.go`;
