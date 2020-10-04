let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "81da868a4ecc9a0baf06b26d712e1c65a251e8fb";
  ribSrc = builtins.fetchTarball {
    url = "https://github.com/srid/rib/archive/${ribRevision}.tar.gz";
    sha256 = "1im732kjw1vmnp0k2gx9qnd9hjy0yk5l2acpqk1ykdg447n0f047";
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
