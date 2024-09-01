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

```bash
watchexec -e hs,hamlet,cabal --restart cabal run -f development
```

# Testing

```bash
watchexec -e hs,cabal --restart cabal test
```

If you want to work on golden tests:

```bash
watchexec --restart "cabal test --test-options=\"--golden-reset --golden-start\""
```
