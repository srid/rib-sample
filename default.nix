let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "a3dc77c4cdae3faef65db7a7d2fedd809b4c4ef7";

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
  fetchGH = repo: rev: builtins.fetchTarball "https://github.com/${repo}/archive/${rev}.tar.gz";
  dsumSrc = fetchGH "mokus0/dependent-sum" "5ab6d81";
  source-overrides = {
    dependent-sum = dsumSrc + "/dependent-sum";
    dependent-sum-template = dsumSrc + "/dependent-sum-template";
    some = fetchGH "phadej/some" "7e2a9ef5352097954a3a416a5ef12bc35b0d53db"; # "1998df3";
  };
in import rib { 
  inherit root name source-overrides; 
}
