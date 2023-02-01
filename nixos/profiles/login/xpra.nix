{ config
, ...
}: {
  services.xserver.displayManager.xpra.enable = true;
  services.xserver.displayManager.xpra.bindTcp = "workstation:10000";
  services.xserver.displayManager.xpra.pulseaudio = true;
}
