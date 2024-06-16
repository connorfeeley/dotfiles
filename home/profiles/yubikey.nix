{ lib, pkgs, inputs, ... }:
let
  inherit (pkgs.stdenv) targetPlatform;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin;
in
{
  # FIXME: fails on aarch64-darwin
  # https://github.com/NixOS/nixpkgs/issues/175875
  # https://github.com/pyca/pyopenssl/issues/873
  home.packages = lib.mkIf (targetPlatform.system != aarch64-darwin) (with pkgs; [
    yubikey-manager
    yubikey-personalization
  ]);

  programs.gpg.scdaemonSettings = {
    disable-ccid = true;
    # Maybe not necessary: only mention is in DrDuh guide for Windows
    reader-port = "Yubico Yubi";
  };

  services.gpg-agent.enableScDaemon = true;
}
