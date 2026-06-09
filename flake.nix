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
          packages = p: [ packages.backend ];
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
          backend = packages.backend-checked;
          frontend = packages.frontend;
        };

        packages = {
          backend-checked = pkgs.haskellPackages.developPackage {
            name = "pablo-tv-backend";
            root = ./backend;
            modifier = drv:
              drv.overrideAttrs
                (oldAttrs: {
                  configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
                  nativeBuildInputs =
                    oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper ] ++ backendRuntimePackages;
                });
          };
          backend-dont-check = pkgs.haskell.lib.dontCheck packages.backend-checked;
          backend = pkgs.haskell.lib.justStaticExecutables packages.backend-dont-check;

          frontend = pkgs.mkElmDerivation {
            name = "pablo-tv-frontend";
            src = ./frontend;
            nativeBuildInputs = frontendBuildPackages;
            buildPhase =
              ''
                # Generate Elm code from backend
                ${packages.backend}/bin/elm-gen --out=src

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

          pablo-tv = pkgs.stdenv.mkDerivation {
            name = "pablo-tv";
            nativeBuildInputs = [ pkgs.makeWrapper ];
            buildInputs = [ packages.backend ];
            # This doesn't really build anything, it just copies the output from backend, and then wraps it.
            # This is super cheap, and doesn't cause the backend to build twice.
            # This way we can have the frontend depend on the backend for the generated code, while adding the output of the frontend as the --frontend flag to the backend, without having a circular dependency and not needing to build the backend twice.
            # buildPhase = "true";
            unpackPhase = "true";
            installPhase = ''
              mkdir -p $out/bin
              cp ${packages.backend}/bin/pablo-tv $out/bin/
              wrapProgram $out/bin/pablo-tv \
                --suffix PATH : ${pkgs.lib.makeBinPath backendRuntimePackages} \
                --add-flags "--frontend=${packages.frontend}"
            '';
          };

          default = packages.pablo-tv;
        };
      }
    );
}
