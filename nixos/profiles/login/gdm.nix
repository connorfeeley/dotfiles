{ config, ... }:
{
  security.pam.services.gdm.enableGnomeKeyring = true;
  services.xserver = {
    displayManager.gdm = {
      enable = true;
      # Don't autosuspend from GDM
      autoSuspend = false;
    };
  };
}
