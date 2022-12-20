# NOTE: derivation is impure
# Tested with XCode CLT version: 14.0.0.0.1.1661618636

let
  macportVersion = "9.1";

  titlebarPatches = false;
in
import ./generic.nix (rec {
  pname = "emacsMacport";
  version = "28.2";
  sha256 = "sha256-7iEYIjPvMjLcl7SGry2G4UBC27ZbvFNd9WLDqFgjJIg=";
  patches =
    if titlebarPatches then fetchpatch: [
      (fetchpatch {
        name = "emacs-mac-title-bar-9.0.patch";
        url = "https://raw.githubusercontent.com/railwaycat/homebrew-emacsmacport/89fc33ae62c82c7ff5894967c1038ef87bb0b418/patches/emacs-mac-title-bar-9.0.patch";
        sha256 = "sha256-TIojIx2TmIGmjPmMNoWHbs9XWCV1DtbRR0jZ3yjsHrc=";
      })
      (fetchpatch {
        name = "emacs-26.2-rc1-mac-7.5-no-title-bar.patch";
        url = "https://raw.githubusercontent.com/railwaycat/homebrew-emacsmacport/667f0efc08506facfc6963ac1fd1d5b9b777e094/patches/emacs-26.2-rc1-mac-7.5-no-title-bar.patch";
        sha256 = "sha256-f2DRcUZq8Y18n6MJ6vtChN5hLGERduMB8B1mrrds6Ns=";
      })
    ] else fetchpatch: [ ];
  macportPatches = builtins.fetchTarball {
    url = "ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-${version}-mac-${macportVersion}.tar.gz";
    sha256 = "0cxrqc8a55fsr3rjw68krndb778m8yq55934fqgz04imcr3yjydf";
  };
})
