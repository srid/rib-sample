# rib-sample

Sample site for the [Rib](https://github.com/srid/rib) static site generator

## Running

First, install the [Nix package manager](https://nixos.org/nix/). Then simply
run:

```bash
nix-shell --run 'ghcid -T main'
```

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `Main.hs` or the content in `./a` reloads everything.

## Use a custom rib and port

You might have a local checkout of rib with certain modifications. And you might
want to run ghcid with the server running at a different port. Both of this can
achieved using the following command:

```bash
# Assuming rib is cloned at ../rib
nix-shell --arg rib ../rib --run 'ghcid -T ":main serve -p 9876"'
```

