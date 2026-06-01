{ config, lib, pkgs, ... }:
let inherit (pkgs.stdenv) isLinux;
in {
  services.vscode-server.enable = lib.mkIf isLinux true;

  home.packages = with pkgs;
    [
      universal-ctags # <- Generates tag files in case of LSP bankrupcy
      # node2nix removed in nixpkgs 26.05 (nodePackages set was also removed).
      # Use buildNpmPackage / buildNpmLockHook etc. directly.

      # NUR repo `mic92.traceshark` is no longer published.
    ] ++ lib.optionals (pkgs.stdenv.isLinux) [
      # NUR repo `mic92.gdb-dashboard` is no longer published.
      bashSnippets # <- Collection of small bash scripts; includes 'cheat' (cheat.sh)
      bubblewrap # <- Believe it or not? Straight to jail.
      (elfutils.override { enableDebuginfod = true; }) # <- Handy tools like eu-stack
      ubootTools # <- tools for working with u-boot images
      ethtool
      rr # Time-travelling debugging tool
    ] ++ lib.optionals (pkgs.stdenv.isLinux && !pkgs.stdenv.isAarch64) [
      # devdocs-desktop removed from nixpkgs 26.05 (unmaintained, insecure deps)
      eclipses.eclipse-java # <- public struct Eclipse { public static void main(String[] args) { System.out.println("Hello World"); } } ... seriously?
    ];
}
