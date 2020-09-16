let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "42bc7c3";
  ribSrc = builtins.fetchTarball {
    url = "https://github.com/srid/rib/archive/${ribRevision}.tar.gz";
    sha256 = "1dph2nar6wfv9krkpcg6imdfjz3vzhwm5624x9kqbslsgvd9lcxx";
  };

  gitignoreSrc = builtins.fetchTarball { 
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { }) gitignoreSource;
in {
# Rib library source to use
  rib ? ribSrc
# Cabal project root
, root ? gitignoreSource ./.
# Cabal project name
, name ? "rib-sample"
, ...
}:

let 
  # Add your Haskell package overrides here
  source-overrides = {
  };
in import rib { 
  inherit root name source-overrides; 
}
