{
  description = "An attempt at building my own weird smart tv interface";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils }:
    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
    in
    {
      overlays.default = final: prev: {
        pablo-tv = prev.haskell.lib.justStaticExecutables (
          final.haskellPackages.pablo-tv.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
          })
        );
        haskellPackages = prev.haskellPackages.override (old: {
          overrides =
            final.lib.composeExtensions
              (old.overrides or (_: _: { }))
              (self: super: {
                pablo-tv =
                  (self.callCabal2nix "pablo-tv" ./. { }).overrideAttrs
                    (oldAttrs: {
                      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ final.makeWrapper final.ydotool ];
                      postInstall =
                        (oldAttrs.postInstall or "")
                        + ''
                          wrapProgram $out/bin/pablo-tv \
                            --suffix PATH : ${final.lib.makeBinPath [final.ydotool]}
                        '';
                    });
              });
        });
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let pkgs = pkgsFor system; in
      rec {
        packages.default = pkgs.pablo-tv;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.default ];
          buildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            hlint
            watchexec
            ydotool
            nil
          ];
          inherit (self.checks.${system}.pre-commit-check) shellHook;
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
        };
      }
    );
}
