import { Machine, Services, mkMachine, MkMachineArg } from "./internal/nixos.ts"

export type Machine = {
    type: "machine",
    nixExpression(): string
}

type MkMachineArg = {
    services: Services
}

export function mkMachine(mkr : (arg : MkMachineArg) => void) : Machine {
    const services = new Services()
    mkr({services})
    return {
      type: "machine",
      nixExpression(): string {
         return services.__nixExpression();
      }
    }
}

const _example = mkMachine(({services} : MkMachineArg) => {
    services.nginx = {
        enabled: true,
        port: 8000
    }
})
