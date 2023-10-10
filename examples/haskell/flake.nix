{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gomod2nix-repo.url = "github:nix-community/gomod2nix?rev=f95720e89af6165c8c0aa77f180461fe786f3c21";
  inputs.npmlock2nix-repo = {
    url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, npmlock2nix-repo, gomod2nix-repo }:
    let
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
          helloFromHaskell_pkg =
            (pkgs.haskell.packages.ghc94.callCabal2nix
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

              { })
            // {
              meta.mainProgram = "helloFromHaskell";
            }
          ;
        });
      checks = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          helloFromHaskell_hlint =
            let
              src =
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
              ;
              dev =
                (
                  let
                    expr =
                      (pkgs.haskell.packages.ghc94.callCabal2nix
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

                        { })
                      // {
                        meta.mainProgram = "helloFromHaskell";
                      }
                    ;
                  in
                  (if expr ? env
                  then expr.env
                  else pkgs.mkShell { inputsFrom = [ expr ]; }
                  )
                ).overrideAttrs (finalAttrs: previousAttrs: {
                  nativeBuildInputs =
                    previousAttrs.nativeBuildInputs
                    ++
                    [ pkgs.hlint ];
                })
              ;
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "
        touch \$out
        cp -r ${src} src
        cd src
        ${"hlint *.hs"}
      "
          ;
        });
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          helloFromHaskell =
            (
              let
                expr =
                  (pkgs.haskell.packages.ghc94.callCabal2nix
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

                    { })
                  // {
                    meta.mainProgram = "helloFromHaskell";
                  }
                ;
              in
              (if expr ? env
              then expr.env
              else pkgs.mkShell { inputsFrom = [ expr ]; }
              )
            ).overrideAttrs (finalAttrs: previousAttrs: {
              nativeBuildInputs =
                previousAttrs.nativeBuildInputs
                ++
                [ pkgs.hlint ];
            })
          ;
        });
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" { inherit system; };
        in
        {

          helloFromHaskell = {
            type = "app";
            program = "${
        let dev = pkgs.mkShell {}; in
        pkgs.runCommand "shell-env" {
          buildInputs = dev.buildInputs;
          nativeBuildInputs = dev.nativeBuildInputs;
        } ''
          echo "export PATH=$PATH:$PATH" > $out
          echo ${pkgs.lib.strings.escapeShellArg dev.shellHook} >> $out
          echo ${pkgs.lib.strings.escapeShellArg "${
    (pkgs.haskell.packages.ghc94.callCabal2nix
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

      { })
      // {
        meta.mainProgram = "helloFromHaskell";
      }
  }/bin/helloFromHaskell"} >> $out
          chmod +x $out
        ''
      }";
          };

        });
    };
}
