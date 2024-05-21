# Darwin configuration for Tailscale
# NOTE: can alternatively use open-source CLI-only package with: 'services.tailscale.enable = true'.

{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption;

  cfg = config.programs.tailscale;
in
{
  options.programs.tailscale = {
    enable = mkEnableOption "Install Tailscale from the Mac App Store.";
  };

  config = mkIf cfg.enable {
    homebrew.masApps = { "Tailscale" = 1475387142; };

    networking.knownNetworkServices = [ "Tailscale Tunnel" ];
  };
}
