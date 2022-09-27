{ config
, lib
, pkgs
, ...
}: {
  # NOTE: see https://source.mcwhirter.io/craige/mio-ops/src/branch/consensus/profiles/xmonad.nix
  # for an example config.
  programs.ssh.askPassword = pkgs.lib.mkForce "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  programs.kdeconnect.enable = true;
  programs.dconf.enable = true;
  services.xserver = {
    enable = true;
    layout = "dvorak";
    libinput.enable = false;
    displayManager.lightdm.enable = true; # Enable the LightDM display manager
    desktopManager.plasma5 = {
      enable = true;
      supportDDC = true;
      useQtScaling = true;
      runUsingSystemd = true;
    };
  };
}
