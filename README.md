Pablo TV
--------

# How to run

```bash
nix run
```

Then open the shown address on your phone.

Note: Make sure to open up the 8080 port so your phone can connect.

# Dev server

```bash
watchexec -e hs,hamlet,cabal --restart cabal run -f development
```

# Testing

```bash
watchexec --restart "cabal test --test-options=\"--golden-reset --golden-start\""
```
