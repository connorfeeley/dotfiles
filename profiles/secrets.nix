{ config
, lib
, pkgs
, inputs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux system;

  cfg = config.age;
  secretsDir = ../secrets;

  # nix-darwin does not support the `users.<name>.extraGroups` option, but
  # that's not a problem since we're only using darwin systems as a single
  # admin user. although the username may vary across systems, each "primary
  # user" will still be in the `admin` group.
  secretsGroup =
    if isLinux
    then "secrets"
    else "admin";

  mkSopsSecret = name: {
    "${name}" = {
      mode = "0440";
      owner = config.users.users.cfeeley.name;
      group = secretsGroup;
    };
  };

  mkEspansoMatchesSecret = name: {
    "espanso/${name}.yml" = {
      file = "${secretsDir}/espanso/${name}.yml.age";
      group = secretsGroup;
      path = "${cfg.secretsDir}/espanso/${name}.yml";
    };
  };
in
{
  environment.sessionVariables = {
    SOPS_AGE_KEY_FILE = "$HOME/.config/sops/age/keys.txt";
    sopsCreateGPGHome = "1";
  };
  home-manager.sharedModules = [{
    # Allow running sops as a normal user without sudo.
    home.sessionVariables = {
      SOPS_AGE_KEY_FILE = "$HOME/.config/sops/age/keys.txt";
      sopsCreateGPGHome = "1";
    };
  }];

  environment.systemPackages = with pkgs; [
    age
    agenix
    rage
    sops
  ];

  users.groups.secrets.members = [ "root" "cfeeley" ];

  # age.secrets = lib.mkMerge [
  #   (mkEspansoMatchesSecret "personal")
  #   # (mkEspansoMatchesSecret "work")
  # ];

  sops.defaultSopsFile = ../secrets/global.secrets.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  sops.secrets = lib.mkMerge [
    (mkSopsSecret "hcloud-tokens/cfeeley-sops")
  ];
}
