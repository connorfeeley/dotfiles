{ config
, lib
, pkgs
, ...
}: {
  services.xserver.displayManager = {
    sddm.enable = true;
    autoLogin = {
      enable = true;
      user = "cfeeley";
    };
    defaultSession = "none+fake";
    session = [{
      manage = "window";
      name = "fake";
      start = "";
    }];
  };
}
