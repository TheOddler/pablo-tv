{
  description = "An attempt at building my own weird smart tv interface";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        devLibs = [
          pkgs.cabal-install
          pkgs.haskell-language-server
          pkgs.hlint
          pkgs.watchexec
          pkgs.nil
          pkgs.haskellPackages.weeder
          pkgs.haskellPackages.cabal-gild
        ];
        runtimeLibs = [
          pkgs.libevdev
          pkgs.xdg-utils
          pkgs.gtk3
        ];
      in
      rec {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [
            # Use the base version because it doesn't have optimisations enabled
            packages.backendBase
          ];
          buildInputs = devLibs ++ runtimeLibs;
          withHoogle = true;
          inherit (checks.pre-commit-check) shellHook;
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              hlint = {
                enable = true;
                settings.hintFile = ./.hlint.yaml;
              };
              ormolu = {
                enable = true;
                settings.defaultExtensions = [
                  "GHC2021" # Let Ormolu know we're using GHC2021
                  "LambdaCase"
                  "OverloadedRecordDot"
                  "OverloadedStrings"
                  "StrictData"
                ];
              };
            };
          };
          app = packages.default;
        };

        packages = {
          backendBase = pkgs.haskellPackages.callCabal2nix "backend" ./backend { };
          backendOptimised = pkgs.haskell.lib.justStaticExecutables (
            pkgs.haskell.lib.dontCheck (
              packages.backendBase.overrideAttrs (oldAttrs: {
                configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
              })
            )
          );
          backendWrapped = pkgs.symlinkJoin {
            name = "pablo-tv";
            paths = [ packages.backendOptimised ];

            nativeBuildInputs = [ pkgs.makeWrapper ];

            postBuild = ''
              wrapProgram $out/bin/pablo-tv \
                --suffix PATH : ${pkgs.lib.makeBinPath runtimeLibs}
            '';
          };
          default = packages.backendWrapped;
        };
      }
    );
}
