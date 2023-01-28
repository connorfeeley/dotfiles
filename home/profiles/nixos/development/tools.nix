{ lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux;
in
{
  services.vscode-server.enable = lib.mkIf isLinux true;

  home.packages = with pkgs; [
    universal-ctags #  <- Generates tag files in case of LSP bankrupcy

    nur.repos.mic92.gdb-dashboard
    nur.repos.mic92.traceshark
  ] ++ lib.optionals (pkgs.stdenv.isLinux) [
    bashSnippets #     <- Collection of small bash scripts; includes 'cheat' (cheat.sh)
    elfutils #         <- Handy tools like eu-stack
    ubootTools #       <- tools for working with u-boot images
    ethtool
  ] ++ lib.optionals (pkgs.stdenv.isLinux && !pkgs.stdenv.isAarch64) [
    devdocs-desktop #  <- full-featured desktop app for DevDocs.io
    eclipses.eclipse-java # <- public struct Eclipse { public static void main(String[] args) { System.out.println("Hello World"); } } ... seriously?
    sourcetrail #      <- Rest in peace sourcetrail, the best C++ exploration tool ever to live.
  ];
}
