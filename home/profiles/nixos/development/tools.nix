{ lib
, pkgs
, ...
}:
lib.mkIf pkgs.stdenv.isLinux {
  services.vscode-server.enable = true;

  home.packages = with pkgs; [
    sourcetrail #      <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
    ubootTools #       <- tools for working with u-boot images
    universal-ctags #  <- Generates tag files in case of LSP bankrupcy
    elfutils #         <- Handy tools like eu-stack

    nur.repos.mic92.gdb-dashboard
    nur.repos.mic92.traceshark
  ] ++ lib.optionals (!pkgs.stdenv.isAarch64) [
    devdocs-desktop #  <- full-featured desktop app for DevDocs.io
  ];
}
