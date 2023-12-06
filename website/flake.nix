{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.npmlock2nix-repo = { url = "github:nix-community/npmlock2nix?rev=9197bbf397d76059a76310523d45df10d2e4ca81"; flake = false; };
  inputs.importedFlake-github_martinvonz_jj_v0_11_0.url = "github:martinvonz/jj/v0.11.0";
  outputs = { self, nixpkgs-repo, npmlock2nix-repo, importedFlake-github_martinvonz_jj_v0_11_0 }:
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
          "website/node_modules" =
            let
              npmlock2nix = import npmlock2nix-repo {
                inherit pkgs;
              };
              pkgs = import nixpkgs-repo {
                config.permittedInsecurePackages = [ ];
                inherit system;
              };
            in
            npmlock2nix.v2.node_modules
              {
                src = (
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
                );
                nodejs = pkgs.nodejs-18_x;
              };
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
          "website/tsc" =
            let
              dev = ((pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.nodejs-18_x) ];
              })).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.nodePackages.typescript-language-server) (pkgs.nodePackages.prettier) ];
              });
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "${"mkdir -p \$out"}
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
    })}/. .
    chmod -R u+rwX .
  "}
      ${"
      echo copying node_modules
      cp -r ${let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import nixpkgs-repo {
        config.permittedInsecurePackages = [];
        inherit system;
      };
      in
      npmlock2nix.v2.node_modules
        {
          src = (let
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
    });
          nodejs = pkgs.nodejs-18_x;
        }}/node_modules .
      chmod -R u+rwX node_modules
    "}
    "}
${"npm run tsc"}
";
          "website/fmt-check" =
            let
              dev = ((pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.nodejs-18_x) ];
              })).overrideAttrs (finalAttrs: previousAttrs: {
                nativeBuildInputs =
                  previousAttrs.nativeBuildInputs
                  ++
                  [ (pkgs.nodePackages.typescript-language-server) (pkgs.nodePackages.prettier) ];
              });
            in
            pkgs.runCommand "check"
              {
                buildInputs = dev.buildInputs ++ dev.nativeBuildInputs;
              } "${"mkdir -p \$out"}
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
    })}/. .
    chmod -R u+rwX .
  "}
      ${"
      echo copying node_modules
      cp -r ${let
        npmlock2nix = import npmlock2nix-repo {
          inherit pkgs;
        };
        pkgs = import nixpkgs-repo {
        config.permittedInsecurePackages = [];
        inherit system;
      };
      in
      npmlock2nix.v2.node_modules
        {
          src = (let
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
    });
          nodejs = pkgs.nodejs-18_x;
        }}/node_modules .
      chmod -R u+rwX node_modules
    "}
    "}
${"prettier '**/*.{ts,tsx}' --check"}
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
          "devEnv" = (pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [
                (
                  let
                    x = importedFlake-github_martinvonz_jj_v0_11_0;
                  in
                  if x ? "packages".${system}."default"
                  then x."packages".${system}."default"
                  else builtins.throw "The package \"${"default"}\" was not found in ${"github:martinvonz/jj/v0.11.0"}"
                )
              ];
          });
          "website" = ((pkgs.mkShell { }).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.nodejs-18_x) ];
          })).overrideAttrs (finalAttrs: previousAttrs: {
            nativeBuildInputs =
              previousAttrs.nativeBuildInputs
              ++
              [ (pkgs.nodePackages.typescript-language-server) (pkgs.nodePackages.prettier) ];
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
          "build" = {
            "type" = "app";
            "program" = "${let
        dev = ((pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        })).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodePackages.typescript-language-server) (pkgs.nodePackages.prettier)];
        });
        shell = "npm install ; npm run build";
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
          "dev" = {
            "type" = "app";
            "program" = "${let
        dev = ((pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        })).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodePackages.typescript-language-server) (pkgs.nodePackages.prettier)];
        });
        shell = "npm install ; npm run dev";
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
          "website/fmt" = {
            "type" = "app";
            "program" = "${let
        dev = ((pkgs.mkShell {}).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodejs-18_x)];
        })).overrideAttrs (finalAttrs: previousAttrs: {
          nativeBuildInputs =
            previousAttrs.nativeBuildInputs
            ++
            [(pkgs.nodePackages.typescript-language-server) (pkgs.nodePackages.prettier)];
        });
        shell = "prettier '**/*.{ts,tsx}' --write";
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
