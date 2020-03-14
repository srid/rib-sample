# rib-sample

Sample site for the [Rib](https://github.com/srid/rib) static site generator

## Prerequisites

First, install the [Nix package manager](https://nixos.org/nix/):

``` bash
bash <(curl https://nixos.org/nix/install)
```

Optionally, enable the [Nix cache](https://srid.cachix.org/) if you would like to speed up local builds:

``` bash
# If you do not already have cachix, install it:
nix-env -iA cachix -f https://cachix.org/api/v1/install
# Enable nix cache for rib
cachix use srid
```

## Running

To build and run the site:

```bash
nix-shell --run 'ghcid -T ":main serve"'
```

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `Main.hs` or the content in `./content` reloads everything.

## Use a custom rib and port

You might have a local checkout of rib with certain modifications. And you might
want to run ghcid with the server running at a different port. Both of this can
achieved using the following command:

```bash
# Assuming rib is cloned at ../rib
nix-shell --arg rib ../rib --run 'ghcid -T ":main serve -p 9876"'
```

## Building the executable

A fully built executable can be produced using `nix-build`:

```
$ nix-build 
...
$ ./result/bin/rib-sample --help
Usage: rib-sample COMMAND
  Rib static site generator CLI

Available options:
  -h,--help                Show this help text

Available commands:
  generate                 Run one-off generation of static files
  watch                    Watch the source directory, and generate when it
                           changes
  serve                    Like watch, but also starts a HTTP server
```
