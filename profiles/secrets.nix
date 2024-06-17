{ config, lib, pkgs, ... }:
let

  inherit (config.lib.dotfiles.secrets) secretsDir secretsGroup;
in
{
  # FIXME: had 'config.dotfiles.guardian.username' in place of cfeeley,
  # but that was causing eval errors when building 'workstation-iso'.
  users.groups.secrets.members = [ "root" "cfeeley" ];
}
