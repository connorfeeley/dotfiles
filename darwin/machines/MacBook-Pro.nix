{ self, self', system, config, pkgs, lib, inputs', ... }:
let
  inherit (config.networking) hostName;

  inherit (config.lib.dotfield.secrets) secretsDir secretsGroup;

  inherit (self.collective) hmArgs;
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

  # home-manager.modules = [
  #   self.inputs.nix-colors.homeManagerModules.default
  #   self.inputs.sops-nix.homeManagerModules.sops
  #   self.inputs.nix-colors.homeManagerModule
  #   self.inputs.nixos-vscode-server.nixosModules.home
  #   self.inputs.nix-index-database.hmModules.nix-index
  #   self.inputs.plasma-manager.homeManagerModules.plasma-manager
  # ];
  home-manager.users = {
    "${config.dotfield.guardian.username}" = {
      imports = with hmArgs.roles;
        (lib.flatten [
          hmArgs.profiles.core
          (_: { imports = [ ../../lib/home ]; })
          hmArgs.modules
        ] ++ [
          self.inputs.nur.hmModules.nur

          self.inputs.nix-colors.homeManagerModules.default
          self.inputs.sops-nix.homeManagerModules.sops
          self.inputs.nix-colors.homeManagerModule
          self.inputs.nixos-vscode-server.nixosModules.home
          self.inputs.nix-index-database.hmModules.nix-index
        ]
        ++ (with hmArgs.profiles; [ shells.fish desktop.vnc ]) ++
        (with hmArgs.roles;
        workstation ++ macos ++ developer ++ emacs-config
          ++ (with hmArgs.profiles; [ work media sync aws ])));

      # imports = [ ../../home/modules/iterm2.nix ];
      _module.args.inputs = self.inputs;

      home = {
        username = "cfeeley";
        homeDirectory = lib.mkForce "/Users/cfeeley";
        stateVersion = "22.05";
      };
      programs.iterm2.enable = true;
    };
  };

  networking.hostName = "MacBook-Pro";

  networking.knownNetworkServices = [ "Wi-Fi" "iPhone USB" "Thunderbolt Bridge" ];

  # Tailscale MAS App
  programs.tailscale.enable = true;

  # Tailscale (open-source, CLI-only)
  # services.tailscale.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

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
