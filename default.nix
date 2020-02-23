let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "66e3a3a";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? gitignoreSource ./.
# Cabal project name
, name ? "rib-sample"
, ...
}:

let 
  fetchGL = owner: repo: rev:
    builtins.fetchTarball ("https://gitlab.com/" + owner + "/" + repo + "/-/archive/" + rev + "/" + repo + "-" + rev + ".tar.gz");
  source-overrides = {
    html-do = fetchGL "sridca" "html-do" "5ad00823e6fbf81f83c260c7c2fb0c8a4c37a2cd";
    # html-do = ../html-do;
    
  };
in import rib { 
  inherit root name source-overrides; 
}
