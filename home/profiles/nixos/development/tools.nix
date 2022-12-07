{ lib
, pkgs
, ...
}:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = with pkgs; [
    sourcetrail #     <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
    ubootTools #      <- tools for working with u-boot images
    universal-ctags # <- Generates tag files in case of LSP bankrupcy
    elfutils # <- Handy tools like eu-stack
  ];
}
