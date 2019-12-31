{
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/eadcdfd.tar.gz"
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "rib-sample"
, ...
}:

import rib { inherit root name; }
