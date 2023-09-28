import * as garnerConfig from "./garner.ts";
import { isCheck, isFormattable, isProject } from "../../ts/base.ts";
import { isRunnable } from "../../ts/runnable.ts";

console.log(`all formatters:`);
for (const [name, value] of Object.entries(garnerConfig)) {
  if (isFormattable(value)) {
    console.log(`${name}`);
  }
}

console.log(`\nall dev servers:`);
for (const [name, value] of Object.entries(garnerConfig)) {
  if (isProject(value) && "dev" in value.runnables) {
    console.log(`${name}`);
  }
}

console.log(`\nall prod servers:`);
for (const [name, value] of Object.entries(garnerConfig)) {
  if (isProject(value) && "prod" in value.runnables) {
    console.log(`${name}`);
  }
}

console.log(`\nall checks:`);
for (const [name, project] of Object.entries(garnerConfig)) {
  if (isProject(project)) {
    for (const checkName of Object.keys(project.checks)) {
      console.log(`${name} -> "${checkName}"`);
    }
  } else if (isCheck(project)) {
    console.log(name);
  }
}

console.log(`\nall runnables:`);
for (const [name, project] of Object.entries(garnerConfig)) {
  if (isProject(project)) {
    for (const runnableName of Object.keys(project.runnables)) {
      console.log(`${name} -> "${runnableName}"`);
    }
  } else if (isRunnable(project)) {
    console.log(name);
  }
}
