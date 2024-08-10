{
  description = "An attempt at building my own weird smart tv interface";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      rec {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [
            # Use the base version because it doesn't have optimisations enabled
            packages.pablo-tv-base
          ];
          buildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            watchexec
            libevdev
            nil
          ];
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
          pablo-tv-base = pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv:
              drv.overrideAttrs
                (oldAttrs: {
                  nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper pkgs.libevdev ];
                  postInstall =
                    (oldAttrs.postInstall or "")
                    + ''
                      wrapProgram $out/bin/pablo-tv \
                        --suffix PATH : ${pkgs.lib.makeBinPath [pkgs.libevdev]}
                    '';
                });
          };
          pablo-tv = pkgs.haskell.lib.justStaticExecutables (
            packages.pablo-tv-base.overrideAttrs (oldAttrs: {
              configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
            })
          );
          default = packages.pablo-tv;
        };
      }
    );
}
