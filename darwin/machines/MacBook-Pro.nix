{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, ...
}:
let
  inherit (collective) peers;
  inherit (config.networking) hostName;
in
{
  ### === users ================================================================

  dotfield.guardian = {
    enable = true;
    username = "cfeeley";
  };

  # Get user and group ID
  users.users.cfeeley = {
    uid = 501;
    gid = 20;
  };

  # Add 'users' group
  users.knownGroups = [ "users" ];
  users.groups.users = {
    members = [ "cfeeley" ];
    gid = 1000;
    description = "users group for syncthing compatibility";
  };

  home-manager.users = {
    cfeeley = hmArgs: {
      imports = with hmArgs.roles; workstation ++ macos ++ (with hmArgs.profiles; [
        sync
        work
        ops
      ]);

      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/Users/cfeeley";
    };
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

  programs.amphetamine = {
    enable = true;
    withEnhancer = false;
  };
}
