{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          haskellExecutable = (pkgs.haskell.packages.ghc94.callCabal2nix "mkHaskell-test" ./. { }) // { meta.mainProgram = "garnerTest"; };
          hello = pkgs.hello;
        });
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          haskellExecutable =
            let expr = self.packages.${system}.haskellExecutable;
            in if self.packages.${system}.haskellExecutable ? env
            then self.packages.${system}.haskellExecutable.env
            else
              pkgs.mkShell ({
                inputsFrom = [ self.packages.${system}.haskellExecutable ];
              });
          hello =
            let expr = self.packages.${system}.hello;
            in if self.packages.${system}.hello ? env
            then self.packages.${system}.hello.env
            else
              pkgs.mkShell ({
                inputsFrom = [ self.packages.${system}.hello ];
              });
        });
      formatter = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        pkgs.nixpkgs-fmt);
    };
}
