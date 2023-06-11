{
        inputs.nixpkgs.url = "github:NixOS/nixpkgs";
        outputs = { self, nixpkgs } :
           let
             systems = ["x86_64-linux"];
             forAllSystems = nixpkgs.lib.genAttrs systems;
           in
        {
           packages = forAllSystems (system:
             let pkgs = import "${nixpkgs}" {
               config.allowUnfree = true;
               inherit system;
             };
             in {
                 
    haskellExecutable = (pkgs.haskell.packages.ghc94.callCabal2nix "mkHaskell-test" ./. { } ) // { meta.mainProgram = "garnerTest"; };
    hello = pkgs.hello;
             });
           devShells = forAllSystems (system:
             let pkgs = import "${nixpkgs}" {
               config.allowUnfree = true;
               inherit system;
             };
             in {
                 
     haskellExecutable = self.packages.${system}.haskellExecutable.env;
     hello = pkgs.mkShell({
         inputsFrom = [ self.packages.${system}.hello ];
        });
             });
        };
     }