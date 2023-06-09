{
        inputs.nixpkgs.url = "github:NixOS/nixpkgs";
        outputs = { self, nixpkgs } :
           let
             system = "x86_64-linux";
             pkgs = import "${nixpkgs}" {
               config.allowUnfree = true;
               inherit system;
             };
           in
        {
           
    haskellExecutable = (pkgs.haskell.packages.ghc94.callCabal2nix "mkHaskell-test" ./. { } ).override { } // { meta.mainProgram = "garnerTest"; };
    hello = pkgs.hello;
        };
     }