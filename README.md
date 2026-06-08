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

# Dev

There's a few scripts in the `scripts` folder.
See there.

You probably want:

```
dev.sh
```

## Testing

You probably want

```
test-dev.sh
```
