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
  users.groups.secrets.members = [ "root" config.dotfield.guardian.username ];

  age.secrets = lib.mkMerge [
    (mkAgeSecret "minecraft-rcon-password.txt")
    (mkAgeSecret "tailscale-luks-setup.state")
    (mkAgeSecret "workstation-luks/ssh_host_ed25519_key")
    (mkAgeSecret "workstation-luks/ssh_host_ed25519_key.pub")
    (mkAgeSecret "workstation-luks/ssh_host_rsa_key")
    (mkAgeSecret "workstation-luks/ssh_host_rsa_key.pub")
  ];
}
