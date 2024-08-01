{
  description = "An attempt at building my own weird smart tv interface";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils }:
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        devPackages = with pkgs; [
          haskell-language-server
          hlint
        ];
      in
      {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.default ];
          buildInputs = devPackages;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
            };
            settings = {
              hlint.hintFile = ./.hlint.yaml;
              ormolu.defaultExtensions = [
                "GHC2021" # Let Ormolu know we're using GHC2021
                "LambdaCase"
                "OverloadedRecordDot"
                "OverloadedStrings"
                "StrictData"
              ];
            };
          };
          app = self.packages.${system}.default;
        };

        packages.default = pkgs.haskellPackages.developPackage {
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              haskellPackages.sydtest-discover
            ]);
        };
      }
    );
}
