{ config
, lib
, pkgs
, ...
}: {
  services.xserver = {
    displayManager.gdm = {
      enable = true;
      autoSuspend = false;
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [ hp.xmonad hp.xmonad-contrib hp.xmonad-extras hp.xmonad-config hp.xmobar-config ];
    };
  };
}
