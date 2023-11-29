{
  inputs.nixpkgs-repo.url = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566";
  inputs.call-flake.url = "github:divnix/call-flake";
  outputs = { self, nixpkgs-repo, call-flake }:
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
        { }
      );
      checks = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        { "default/checks" = pkgs.linkFarm "all-checks" (call-flake ./.garn/legacy).checks.${system}; }
      );
      devShells = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        { "default" = pkgs.mkShell { }; }
      );
      apps = forAllSystems (system:
        let
          pkgs = import "${nixpkgs}" {
            config.allowUnfree = true;
            inherit system;
          };
        in
        { "default/fileserver" = { "type" = "app"; "program" = (call-flake ./.garn/legacy).apps.${system}."fileserver".program; }; }
      );
    };
}
