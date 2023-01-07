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

  home-manager.users = {
    "${config.dotfield.guardian.username}" = hmArgs: {
      imports = with hmArgs.roles; workstation ++ macos ++ developer ++ emacs-config ++ (with hmArgs.profiles; [
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

  services.input-leap = {
    enable = true;

    client = {
      enable = true;
      serverHost = "workstation";
    };
  };

  age.secrets = {
    dotfield-readme-update-access-token = { file = "${secretsDir}/dotfield-readme-update-access-token.txt.age"; group = secretsGroup; };
  };
}
