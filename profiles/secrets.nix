{ config
, lib
, pkgs
, inputs
, ...
}:
let
  inherit (pkgs.stdenv) isLinux system;

  cfg = config.age;

  # nix-darwin does not support the `users.<name>.extraGroups` option, but
  # that's not a problem since we're only using darwin systems as a single
  # admin user. although the username may vary across systems, each "primary
  # user" will still be in the `admin` group.
  secretsGroup =
    if isLinux
    then "secrets"
    else "admin";

  secretsDir = "${config.lib.dotfield.fsPath}/secrets/age";
  mkAgeSecret = name: {
    "${name}" = {
      file = "${secretsDir}/${name}.age";
      group = secretsGroup;
      # path = "${cfg.secretsDir}/${name}";
    };
  };
in
{
  users.groups.secrets.members = [ "root" config.dotfield.guardian.username ];

  age.secrets = lib.mkMerge [
    (mkAgeSecret "tailscale-luks-setup.state")
    (mkAgeSecret "workstation-luks/ssh_host_ed25519_key")
    (mkAgeSecret "workstation-luks/ssh_host_ed25519_key.pub")
    (mkAgeSecret "workstation-luks/ssh_host_rsa_key")
    (mkAgeSecret "workstation-luks/ssh_host_rsa_key.pub")
  ];
}
