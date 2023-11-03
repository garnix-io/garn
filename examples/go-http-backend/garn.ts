import * as garn from "http://localhost:8777/mod.ts";

export const server: garn.Project = garn.go
  .mkGoProject({
    description: "example backend server in go",
    src: ".",
    goVersion: "1.20",
  })
  .addExecutable("migrate", "go run ./scripts/migrate.go")
  .addExecutable("dev", "go run ./main.go");
