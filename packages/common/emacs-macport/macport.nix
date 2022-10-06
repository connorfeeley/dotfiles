# NOTE: derivation is impure
# Tested with XCode CLT version: 14.0.0.0.1.1661618636

let
  macportVersion = "9.1";
in import ./generic.nix (rec {
  pname = "emacsMacport";
  version = "28.2";
  sha256 = "sha256-7iEYIjPvMjLcl7SGry2G4UBC27ZbvFNd9WLDqFgjJIg=";
  patches = fetchpatch: [ ];
  inherit macportVersion;
  macportPatches = builtins.fetchTarball {
    url = "ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-${version}-mac-${macportVersion}.tar.gz";
    sha256 = "0cxrqc8a55fsr3rjw68krndb778m8yq55934fqgz04imcr3yjydf";
  };
})
