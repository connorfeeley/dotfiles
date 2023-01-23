{ config
, ...
}: {
  services.xserver = {
    displayManager = {
      sddm.enable = true;
      sddm.enableHidpi = true;
      # sddm.settings = { Autologin = { Session = "plasma.desktop"; User = "john"; } ; };
    };
  };
}
