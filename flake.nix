{
  description = "An attempt at building my own weird smart tv interface";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, mkElmDerivation }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [ mkElmDerivation.overlays.default ];
          inherit system;
        };

        devPackages = with pkgs; [
          watchexec # automatically re-run stuff on change
          parallel # run a bunch of processes in one terminal during dev
          nil # manages nix

          # Haskell stuff
          cabal-install
          haskell-language-server
          hlint
          haskellPackages.weeder
          haskellPackages.cabal-gild

          # Elm things
          elmPackages.elm-format
          elmPackages.elm-json
          elmPackages.elm-review
          elmPackages.nodejs
        ];

        frontendBuildPackages = with pkgs; [
          elmPackages.elm
          terser
          dart-sass
        ];
        backendRuntimePackages = with pkgs; [
          libevdev
          xdg-utils
          gtk3
        ];
      in
      rec {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [
            # Use the base version because it doesn't have optimisations enabled
            packages.pablo-tv-base
          ];
          buildInputs =
            devPackages ++ backendRuntimePackages ++ frontendBuildPackages;
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
                settings.hintFile = ./backend/.hlint.yaml;
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
            root = ./backend;
            modifier = drv:
              drv.overrideAttrs
                (oldAttrs: {
                  nativeBuildInputs =
                    oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper ] ++ backendRuntimePackages;
                  postInstall =
                    (oldAttrs.postInstall or "")
                    + ''
                      wrapProgram $out/bin/pablo-tv \
                        --suffix PATH : ${pkgs.lib.makeBinPath backendRuntimePackages}
                    '';
                });
          };
          pablo-tv = pkgs.haskell.lib.justStaticExecutables (
            pkgs.haskell.lib.dontCheck (
              packages.pablo-tv-base.overrideAttrs (oldAttrs: {
                configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
                postInstall =
                  (oldAttrs.postInstall or "")
                  + ''
                    wrapProgram $out/bin/pablo-tv \
                      --add-flags "--frontend=${packages.frontend}"
                  '';
              })
            )
          );

          frontend = pkgs.mkElmDerivation {
            name = "pablo-tv-frontend";
            src = ./frontend;
            nativeBuildInputs = frontendBuildPackages;
            buildPhase =
              ''
                # Generate Elm code from backend
                ${packages.pablo-tv-base}/bin/elm-gen --out=src

                # Build Elm
                elm make src/Main.elm --output=main.js --optimize
                # Optimise it further
                terser main.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output main.js

                # Build CSS
                sass css/main.scss static/main.css
              '';
            installPhase =
              ''
                mkdir $out
                cp main.js $out
                cp -r static/. $out
              '';
          };

          default = packages.pablo-tv;
        };
      }
    );
}
