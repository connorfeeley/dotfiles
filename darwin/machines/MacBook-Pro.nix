{ config
, lib
, profiles
, collective
, ...
}:
let
  inherit (config.networking) hostName;

  inherit (config.lib.dotfield.secrets) secretsDir secretsGroup;
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
      imports = with hmArgs.roles; workstation ++ macos ++ developer ++ emacs-config ++ (with hmArgs.profiles; [
        sync
        work
      ]);

      home.username = hmArgs.lib.mkForce "cfeeley";
      home.homeDirectory = hmArgs.lib.mkForce "/Users/cfeeley";

      programs.iterm2.enable = true;
    };
  };

  networking.hostName = "MacBook-Pro";

  # FIXME: verify
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Wi-Fi"
    "iPhone USB"
    "Thunderbolt Bridge"
  ];

  # Tailscale MAS App
  programs.tailscale.enable = true;

  programs.amphetamine = {
    enable = true;
    withEnhancer = true;
  };

  age.secrets = {
    dotfield-readme-update-access-token = { file = "${secretsDir}/dotfield-readme-update-access-token.txt.age"; group = secretsGroup; };
  };
}
