{
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/11a9f5d.tar.gz"
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "rib-sample"
, ...
}:

import rib { inherit root name; }
