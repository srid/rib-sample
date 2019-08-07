{ pkgs ? import <nixpkgs> {}
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "rib-sample"
# Rib library source to use
, rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/3f6dc48.tar.gz"
, ...
}:

import rib { inherit pkgs root name; }
