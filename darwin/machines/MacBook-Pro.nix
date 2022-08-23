{
  config,
  lib,
  pkgs,
  profiles,
  suites,
  inputs,
  primaryUser,
  collective,
  ...
}: let
  inherit (collective) peers;
  inherit (config.networking) hostName;
in {
  ### === users ================================================================

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cfeeley";

  home-manager.users = {
    cfeeley = hmArgs: {imports = with hmArgs.roles; workstation;};
  };

  networking.hostName = "MacBook-Pro";

  # FIXME: verify
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Wi-Fi"
    "iPhone USB"
    "Thunderbolt Bridge"
    "Tailscale Tunnel"
  ];
}
