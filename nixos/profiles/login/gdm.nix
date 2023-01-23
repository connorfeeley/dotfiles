{ config, ...}: {
  services.xserver = {

    displayManager.gdm = {
      enable = true;
      # Don't autosuspend from GDM
      autoSuspend = false;
    };
  };
}
