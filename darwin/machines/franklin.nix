{ config
, lib
, profiles
, collective
, ...
}:
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
      imports = with hmArgs.roles; server ++ shell ++ macos ++ (with hmArgs.profiles; [
        sync
        work
        ops
      ]);

      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/Users/cfeeley";

      programs.iterm2.enable = true;
    };
  };

  ### === networking ================================================================

  networking.hostName = "franklin";

  # FIXME: verify
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "USB 10/100/1000 LAN"
    "Wi-Fi"
    "Thunderbolt Bridge"
  ];

  # Open source 'tailscaled' client
  services.tailscale = {
    enable = true;
    domain = "connorfeeley.github";
    magicDNS.enable = true;
  };
  # Use cloudflare DNS for fallback
  networking.dns = [
    "1.1.1.1"
    "1.0.0.1"
    "2606:4700:4700::1111"
    "2606:4700:4700::1001"
  ];

  ### === programs ================================================================

  programs.amphetamine = {
    enable = true;
    withEnhancer = false;
  };
}
