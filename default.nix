let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "03510616ab2e64d58b1d87b69ea357ab2b3b79ed";
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "rib-sample"
, ...
}:

import rib { inherit root name; }
