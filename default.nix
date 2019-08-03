{ pkgs ? import <nixpkgs> {}
# Cabal project root
, root ? ./. 
# Rib library source to use
, rib ? pkgs.fetchFromGitHub {
    owner = "srid";
    repo = "rib";
    rev = "25875b4af22833bf8e6debdd136a3c56d5fd04b4";
    sha256 = "0x9blxpvnl02hj76msli60ffbliqrl23v6gp59rsvi9qgyilcfmc";
  }
, ...
}:

import rib { inherit pkgs root; }
