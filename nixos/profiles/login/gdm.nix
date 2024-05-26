{ config, ... }:
{
  security.pam.services.gdm.enableGnomeKeyring = false;
  services.xserver = {
    displayManager.gdm = {
      enable = true;
      wayland = true;
      # Don't autosuspend from GDM
      autoSuspend = false;
    };
  };
}
