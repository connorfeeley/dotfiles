{ config
, lib
, pkgs
, inputs
, collective
, ...
}:
let
  inherit (pkgs.stdenv) isLinux system;
  inherit (config.lib.dotfield.secrets) mkAgeSecret;

  cfg = config.age;
in
{
  # FIXME: had 'config.dotfield.guardian.username' in place of cfeeley,
  # but that was causing eval errors when building 'workstation-iso'.
  users.groups.secrets.members = [ "root" "cfeeley" ];

  age.secrets = lib.mkMerge [
    (mkAgeSecret "minecraft-rcon-password.txt")
    (mkAgeSecret "tailscale-luks-setup.state")
    (mkAgeSecret "workstation-luks/ssh_host_ed25519_key")
    (mkAgeSecret "workstation-luks/ssh_host_ed25519_key.pub")
    (mkAgeSecret "workstation-luks/ssh_host_rsa_key")
    (mkAgeSecret "workstation-luks/ssh_host_rsa_key.pub")
  ];
}
