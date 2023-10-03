{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          haskellExecutable =
            let
              expr =
                (pkgs.haskell.packages.ghc94.callCabal2nix
                  "garner-pkg"

                  (
                    let
                      lib = pkgs.lib;
                      lastSafe = list:
                        if lib.lists.length list == 0
                        then null
                        else lib.lists.last list;
                    in
                    builtins.path
                      {
                        path = ./.;
                        filter = path: type:
                          let
                            fileName = lastSafe (lib.strings.splitString "/" path);
                          in
                          fileName != "flake.nix";
                      }
                  )

                  { })
                // {
                  meta.mainProgram = "garnerTest";
                }
              ;
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            )
          ;
        });
    };
}
