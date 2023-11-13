{ config, pkgs, lib, collective, ... }:
let
  inherit (config.networking) hostName;

  inherit (config.lib.dotfield.secrets) secretsDir secretsGroup;
in
{
  # imports = [ profiles.pulseaudio ];
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
    "${config.dotfield.guardian.username}" = {
      # imports = with hmArgs.roles;
      #   (hmArgs.lib.flatten [ ]
      #   ++ (hmArgs.lib.flatten [ shell developer emacs-config graphical server trusted webdev fpgadev linux ]))
      #   ++ (with hmArgs.profiles; [ shells.fish desktop.vnc ]) ++
      # (with hmArgs.roles;
      # workstation ++ macos ++ developer ++ emacs-config
      # ++ (with hmArgs.profiles; [ work media sync aws ]));
      imports = [ ../../home/modules/iterm2.nix ];

      home = {
        username = "cfeeley";
        homeDirectory = lib.mkForce "/Users/cfeeley";
        stateVersion = "22.05";
      };
      programs.iterm2.enable = true;
    };
  };

  networking.hostName = "MacBook-Pro";

  networking.knownNetworkServices =
    [ "Wi-Fi" "iPhone USB" "Thunderbolt Bridge" ];

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
      serverAddress = "workstation";
    };
  };

  age.secrets = {
    dotfield-readme-update-access-token = {
      file = "${secretsDir}/dotfield-readme-update-access-token.txt.age";
      group = secretsGroup;
    };
  };

  homebrew.casks = [
    { name = "malwarebytes"; }
    { name = "trader-workstation"; } # IBKR TWS
    { name = "inkscape"; }
  ];
}
