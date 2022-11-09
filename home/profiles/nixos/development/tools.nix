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
    sourcetrail # <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
    ubootTools #  <- tools for working with u-boot images
  ];

  programs.vscode = {
    enable = true;
    # Add extension-specific dependencies needed for rust lang server and rust-analyzer extension
    package = pkgs.vscode.fhsWithPackages (ps: with ps; [ rustup zlib openssl.dev pkg-config ]);
  };
}
