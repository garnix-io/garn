{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  outputs = { self, nixpkgs-repo }:
    let
      nixpkgs = nixpkgs-repo;
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
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
          "helloFromHaskell/pkg" = (pkgs.haskell.packages.ghc94.callCabal2nix
            "garn-pkg"
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
                  name = "source";
                  filter = path: type:
                    let
                      fileName = lastSafe (lib.strings.splitString "/" path);
                    in
                    fileName != "flake.nix" &&
                    fileName != "garn.ts";
                }
            )
            { });
        }
      );
      checks = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "helloFromHaskell/hlint" =
            let
              dev = ((
                let
                  expr = (pkgs.haskell.packages.ghc94.callCabal2nix
                    "garn-pkg"
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
                          name = "source";
                          filter = path: type:
                            let
                              fileName = lastSafe (lib.strings.splitString "/" path);
                            in
                            fileName != "flake.nix" &&
                            fileName != "garn.ts";
                        }
                    )
                    { });
                in
                (if expr ? env
                then expr.env
                else pkgs.mkShell { inputsFrom = [ expr ]; }
                )
              ).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.haskell.packages.ghc94.cabal-install) ];
              })).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.hlint) ];
              });
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "
    touch \$out
    ${"
      ${"
    echo copying source
    cp -r ${(let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./.;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })} src
    chmod -R u+rwX src
    cd src
  "}
      ${""}
    "}
    ${"hlint *.hs"}
  ";
        }
      );
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "helloFromHaskell" = ((
            let
              expr = (pkgs.haskell.packages.ghc94.callCabal2nix
                "garn-pkg"
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
                      name = "source";
                      filter = path: type:
                        let
                          fileName = lastSafe (lib.strings.splitString "/" path);
                        in
                        fileName != "flake.nix" &&
                        fileName != "garn.ts";
                    }
                )
                { });
            in
            (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
            )
          ).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.haskell.packages.ghc94.cabal-install) ];
          })).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.hlint) ];
          });
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "helloFromHaskell/hello" = {
            "type" = "app";
            "program" = "${let
        dev = let expr = (pkgs.haskell.packages.ghc94.callCabal2nix
        "garn-pkg"
        (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./.;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
        { });
        in
          (if expr ? env
            then expr.env
            else pkgs.mkShell { inputsFrom = [ expr ]; }
          );
        shell = "${(pkgs.haskell.packages.ghc94.callCabal2nix
        "garn-pkg"
        (let
    lib = pkgs.lib;
    lastSafe = list :
      if lib.lists.length list == 0
        then null
        else lib.lists.last list;
  in
  builtins.path
    {
      path = ./.;
      name = "source";
      filter = path: type:
        let
          fileName = lastSafe (lib.strings.splitString "/" path);
        in
         fileName != "flake.nix" &&
         fileName != "garn.ts";
    })
        { })}/bin/${"hello"}";
        buildPath = pkgs.runCommand "build-inputs-path" {
          inherit (dev) buildInputs nativeBuildInputs;
        } "echo $PATH > $out";
      in
      pkgs.writeScript "shell-env"  ''
        #!${pkgs.bash}/bin/bash
        export PATH=$(cat ${buildPath}):$PATH
        ${dev.shellHook}
        ${shell} "$@"
      ''}";
          };
        }
      );
    };
}
