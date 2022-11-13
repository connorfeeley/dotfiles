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
  ];
}
