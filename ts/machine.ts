
class Services {
   private config: Record<string, Object>;

   constructor() {
       this.config = {}
   }

   set nginx(args : {enabled: boolean, port: number}) {
       this.config.nginx = args
   }
}
type MkMachineArg = {
    services: Services
}

export function mkMachine(_mkr : (arg : MkMachineArg) => void) : void {

}


export const myMachine = mkMachine(arg  => {
    arg.services.nginx = {
        enabled: true,
        port: 8000
    }
}

