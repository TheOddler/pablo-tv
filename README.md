Pablo TV
--------

# How to run

```bash
nix run
```

Then open the shown address on your phone.

Note: Make sure to open up the 8080 port so your phone can connect.

Alternatively, without the need to clone:

```bash
nix run github:TheOddler/pablo-tv
```

## Installation

Add something like this to your system flake:

```nix
{
  inputs = {
    pablo-tv.url = "github:TheOddler/pablo-tv";
  };
  outputs = { nixpkgs, pablo-tv, ... }:
    let system = "x86_64-linux";
    in {
      nixosConfigurations.youSystem = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          { environment.systemPackages = [ pablo-tv.packages.${system}.default ]; }
        ];
      };
    };
}
```

# Dev server

backend:

```bash
watchexec -e hs,hamlet,cabal --restart cabal run backend -f development
```

frontend:

```bash
(cd frontend && elm-live src/Main.elm --dir=static --proxy-prefix=/api --proxy-host=http://localhost:8080 -- --output=static/main.js)
```

# Testing

```bash
watchexec -e hs,cabal --restart cabal test backend
```

Or test and run:

```bash
watchexec -e hs,hamlet,cabal --restart "cabal test backend -f development && cabal run backend -f development"
```

If you want to work on golden tests:

```bash
watchexec -e hs,cabal --restart "cabal test backend --test-options=\"--golden-reset --golden-start\""
```

Or want to run only a specific test:

```bash
watchexec -e hs,cabal --restart "cabal test backend --test-options=\"--match \\\"TEST_NAME_HERE\\\"\""
```
