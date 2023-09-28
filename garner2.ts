import garner from "...";

export const goBackend = garner.mkGoProject({
  src: "goBackend",
  bins: {
    server: "src/server.go",
    migrate: "utils/migrator.go",
  },
}).withTasks({
  codeGen: garner.bash`${garner.pkgs.protoc} generate protobufs/*.proto`,
}).withGeneratorCheck("codeGen");
