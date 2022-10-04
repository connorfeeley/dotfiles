let
  macportVersion = "9.1";
in import ./generic.nix (rec {
  version = "28.2";
  sha256 = "sha256-4oSLcUDR0MOEt53QOiZSVU8kPJ67GwugmBxdX3F15Ag=";
  patches = fetchpatch: [
    # (fetchpatch {
    #   name = "emacs-mac-title-bar-9.0.patch";
    #   url = "https://raw.githubusercontent.com/railwaycat/homebrew-emacsmacport/master/patches/emacs-mac-title-bar-9.0.patch";
    #   sha256 = "12y6rzd97bn2wy39zx5d2dshs6cblz6ds58kjfvlnx7p5flrswac";
    # })
    # (fetchpatch {
    #   name = "fix-aarch64-darwin-triplet.patch";
    #   url = "https://git.savannah.gnu.org/cgit/emacs.git/patch/?id=a88f63500e475f842e5fbdd9abba4ce122cdb082";
    #   sha256 = "sha256-RF9b5PojFUAjh2TDUW4+HaWveV30Spy1iAXhaWf1ZVg=";
    # })
  ];
  inherit macportVersion;
  macportPatches = builtins.fetchTarball {
    url = "ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-${version}-mac-${macportVersion}.tar.gz";
    sha256 = "0cxrqc8a55fsr3rjw68krndb778m8yq55934fqgz04imcr3yjydf";
  };
})
