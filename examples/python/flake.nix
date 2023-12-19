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
          "someproject/package" =
            let
              lib = pkgs.lib;
              python = pkgs.pythonInterpreters.python310;
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
              pyproject = builtins.fromTOML
                (builtins.readFile "${src}/pyproject.toml");
              dependencies = pyproject.project.dependencies or [ ];
              pickPackage = dep: python.pkgs.${dep} or (throw ''
                Package not supported or not available: ${dep}
                Dependency not found in nixpkgs python packages
              '');
              toPackages = map pickPackage;
              pythonDependencies = toPackages dependencies;
              pythonSetupDependencies = toPackages pyproject.build-system.requires or [ "setuptools" ];
            in
            python.pkgs.buildPythonPackage {
              pname = pyproject.project.name;
              version = pyproject.project.version;
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
              format = "pyproject";
              buildInputs = pythonSetupDependencies;
              propagatedBuildInputs = pythonDependencies;
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
        { }
      );
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        {
          "someproject" =
            let
              lib = pkgs.lib;
              python = pkgs.pythonInterpreters.python310;
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
              pyproject = builtins.fromTOML
                (builtins.readFile "${src}/pyproject.toml");
              dependencies = pyproject.project.dependencies or [ ];
              pickPackage = dep: python.pkgs.${dep} or (throw ''
                Package not supported or not available: ${dep}
                Dependency not found in nixpkgs python packages
              '');
              toPackages = map pickPackage;
              pythonDependencies = toPackages dependencies;
              pythonSetupDependencies = toPackages pyproject.build-system.requires or [ "setuptools" ];
              pythonWithDeps = python.withPackages (ps:
                pythonDependencies
                ++ pythonSetupDependencies
                ++ [
                  python.pkgs.pip
                  python.pkgs.wheel
                ]
              );
              shellHook = ''
                tmp_path=$(realpath ./.pythonenv)
                mkdir -p "$tmp_path"

                repo_root=$(realpath .)
                mkdir -p "$tmp_path/python/${python.sitePackages}"

                # Install the package in editable mode
                # This allows executing scripts from within the dev-shell using the current
                # version of the code and its dependencies.
                ${pythonWithDeps.interpreter} -m pip install             --quiet             --disable-pip-version-check             --no-index             --no-build-isolation             --prefix "$tmp_path/python"             --editable $repo_root

                export PATH="$tmp_path/python/bin:$PATH"
                export PYTHONPATH="$repo_root:$tmp_path/python/${pythonWithDeps.sitePackages}:"
              '';
            in
            pythonWithDeps.env.overrideAttrs (old: {
              shellHook = old.shellHook or "" + "\n" + shellHook;
            });
          "someproject/devShell" =
            let
              lib = pkgs.lib;
              python = pkgs.pythonInterpreters.python310;
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
              pyproject = builtins.fromTOML
                (builtins.readFile "${src}/pyproject.toml");
              dependencies = pyproject.project.dependencies or [ ];
              pickPackage = dep: python.pkgs.${dep} or (throw ''
                Package not supported or not available: ${dep}
                Dependency not found in nixpkgs python packages
              '');
              toPackages = map pickPackage;
              pythonDependencies = toPackages dependencies;
              pythonSetupDependencies = toPackages pyproject.build-system.requires or [ "setuptools" ];
              pythonWithDeps = python.withPackages (ps:
                pythonDependencies
                ++ pythonSetupDependencies
                ++ [
                  python.pkgs.pip
                  python.pkgs.wheel
                ]
              );
              shellHook = ''
                tmp_path=$(realpath ./.pythonenv)
                mkdir -p "$tmp_path"

                repo_root=$(realpath .)
                mkdir -p "$tmp_path/python/${python.sitePackages}"

                # Install the package in editable mode
                # This allows executing scripts from within the dev-shell using the current
                # version of the code and its dependencies.
                ${pythonWithDeps.interpreter} -m pip install             --quiet             --disable-pip-version-check             --no-index             --no-build-isolation             --prefix "$tmp_path/python"             --editable $repo_root

                export PATH="$tmp_path/python/bin:$PATH"
                export PYTHONPATH="$repo_root:$tmp_path/python/${pythonWithDeps.sitePackages}:"
              '';
            in
            pythonWithDeps.env.overrideAttrs (old: {
              shellHook = old.shellHook or "" + "\n" + shellHook;
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
        { }
      );
    };
}
