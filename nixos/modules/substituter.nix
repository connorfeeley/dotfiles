{ self, config, lib, pkgs, ... }:
let
  inherit (config.lib.dotfiles.secrets) secretsDir;

  inherit (self.collective.peers) networks;

  cfg = config.substituter;

  workstation-nix-sign-key-priv = {
    file = "${secretsDir}/hosts/workstation/cache-priv-key.pem.age";
    group = "nixbld";
  };
in
{
  options.substituter = {
    enable = lib.mkEnableOption "Whether to make the host a Nix substituter.";

    hostName = lib.mkOption {
      type = lib.types.str;
      default = config.networking.hostName;
      description = "Hostname for the substituter.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Generate a new binary cache key on first boot
    systemd.services.generate-nix-cache-key = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      path = [ pkgs.nix ];
      script = ''
        [[ -f /etc/nix/cache-priv-key.pem ]] && exit
        nix-store --generate-binary-cache-key ${config.networking.hostName}.${networks.tailscale.domain}-1 /etc/nix/cache-priv-key.pem /etc/nix/cache-pub-key.pem
      '';
    };

    age.secrets = { inherit workstation-nix-sign-key-priv; };

    nix.settings = {
      # Sign locally-built paths automatically
      secret-key-files = [ config.age.secrets.workstation-nix-sign-key-priv.path ];

      # Trust my own key
      trusted-public-keys = [
        "workstation.elephant-vibes.ts.net:10NdZltvaHQ+R4zJ+Vze7V2sHyy8fo6mq0me2mTrqho="
      ];
    };
  };
}
