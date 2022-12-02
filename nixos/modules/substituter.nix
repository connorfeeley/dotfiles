{ config
, lib
, pkgs
, ...
}:
let

  inherit (config.lib.dotfield.secrets) secretsDir;

  workstation-priv = { file = "${secretsDir}/hosts/workstation/cache-priv-key.pem.age"; group = "nixbld"; };

  cfg = config.substituter;
in
{
  options.substituter = {
    enable = lib.mkEnableOption "Whether to make the host a Nix substituter.";

    hostName = lib.mkOption {
      type = lib.types.str;
      default = "${config.networking.hostName}";
      description = "Hostname for the substituter.";
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets = {
      inherit workstation-priv;
    };
    nix = {
      settings = {
        secret-key-files = [ config.age.secrets.workstation-priv.path ];
      };
    };
  };
}
