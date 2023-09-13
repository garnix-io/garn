{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
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
          haskellExecutable = (pkgs.haskell.packages.ghc94.callCabal2nix "garner-pkg" ./. { }) // { meta.mainProgram = "garnerTest"; };
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
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            );
          hello =
            let expr = self.packages.${system}.hello;
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            );
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
