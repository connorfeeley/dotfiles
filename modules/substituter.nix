{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isDarwin;

  substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://cfeeley.cachix.org"
    "https://nixpkgs-wayland.cachix.org"
    "https://cache.iog.io"
  ];
  trusted-substituters = substituters;

  inherit (config.lib.dotfield.secrets) secretsDir secretsGroup mkAgeSecret;

  workstation-priv = { file = "${secretsDir}/hosts/workstation/cache-priv-key.pem.age"; group = "nixbld"; };
  workstation-pub = { file = "${secretsDir}/hosts/workstation/cache-pub-key.pem.age"; group = "nixbld"; };

  macbook-pro-priv = { file = "${secretsDir}/hosts/macbook-pro/cache-priv-key.pem.age"; group = "nixbld"; };
  macbook-pro-pub = { file = "${secretsDir}/hosts/macbook-pro/cache-pub-key.pem.age"; group = "nixbld"; };

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
