{ config, lib, pkgs, ... }:
let inherit (pkgs.stdenv) isLinux;
in {
  services.vscode-server.enable = lib.mkIf isLinux true;

  home.packages = with pkgs;
    [
      universal-ctags # <- Generates tag files in case of LSP bankrupcy
      node2nix # <- generate nix derivations from NPM packages

      config.nur.repos.mic92.traceshark
    ] ++ lib.optionals (pkgs.stdenv.isLinux) [
      config.nur.repos.mic92.gdb-dashboard
      bashSnippets # <- Collection of small bash scripts; includes 'cheat' (cheat.sh)
      bubblewrap # <- Believe it or not? Straight to jail.
      (elfutils.override { enableDebuginfod = true; }) # <- Handy tools like eu-stack
      ubootTools # <- tools for working with u-boot images
      ethtool
      rr # Time-travelling debugging tool
    ] ++ lib.optionals (pkgs.stdenv.isLinux && !pkgs.stdenv.isAarch64) [
      devdocs-desktop # <- full-featured desktop app for DevDocs.io
      eclipses.eclipse-java # <- public struct Eclipse { public static void main(String[] args) { System.out.println("Hello World"); } } ... seriously?
    ];
}
