{
  config,
  lib,
  pkgs,
  ...
}: {
  services.syncthing = {
    enable = true;
    tray.enable = false;
  };
}
