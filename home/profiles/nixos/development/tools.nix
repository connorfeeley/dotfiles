{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = with pkgs; [
    sourcetrail #     <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
    ubootTools #      <- tools for working with u-boot images
    universal-ctags # <- Generates tag files in case of LSP bankrupcy
    elfutils # <- Handy tools like eu-stack
  ];

  programs.vscode = {
    enable = false;
    # Add extension-specific dependencies needed for rust lang server and rust-analyzer extension
    package = pkgs.vscode.fhsWithPackages (ps: with ps; [ rustup zlib openssl.dev pkg-config ]);
  };
}
