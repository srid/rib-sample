# rib-sample

Sample site for the Rib static site generator

## Running

First, install the [Nix package manager](https://nixos.org/nix/). Then simply
run:

```
nix-shell --run 'ghcid -T main'
```

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `Main.hs` or the content in `./a` reloads everything.

