let
  macportVersion = "9.0";
in import ./generic.nix (rec {
  version = "28.1";
  sha256 = "sha256-D33wnlxhx0LyG9WZaQDj2II3tG0HcJdZTC4dSA3lrgY=";
  patches = fetchpatch: [
    # (fetchpatch {
    #   name = "fix-aarch64-darwin-triplet.patch";
    #   url = "https://git.savannah.gnu.org/cgit/emacs.git/patch/?id=a88f63500e475f842e5fbdd9abba4ce122cdb082";
    #   sha256 = "sha256-RF9b5PojFUAjh2TDUW4+HaWveV30Spy1iAXhaWf1ZVg=";
    # })
  ];
  inherit macportVersion;
  macportPatches = builtins.fetchTarball {
    url = "ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-${version}-mac-${macportVersion}.tar.gz";
    sha256 = "03z9wrpqqmdpx3y3q73ybpjpilrmrhcykwv5ac7yrhfn2rh6lj7i";
  };
})
