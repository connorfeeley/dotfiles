{ config
, lib
, pkgs
, ...
}: {
  # NOTE: see https://source.mcwhirter.io/craige/mio-ops/src/branch/consensus/profiles/xmonad.nix
  # for an example config.
  programs.ssh.askPassword = pkgs.lib.mkForce "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  programs.kdeconnect.enable = true;
  services.xserver = {
    enable = true;
    layout = "dvorak";
    libinput.enable = false;
    displayManager = {
      defaultSession = "plasma"; # Set KDE configured to use xmonad as default
      # defaultSession = "none+xmonad"; # Set KDE configured to use xmonad as default
      gdm.enable = false; # Enable the GNOME display manager
      sddm.enable = false; # Enable the Plasma display manager
      lightdm.enable = true; # Enable the LightDM display manager
    };
    desktopManager.plasma5 = {
      enable = true;
      supportDDC = true;
      useQtScaling = true;
    };
    # windowManager.xmonad = {
    #   enable = true;
    #   enableContribAndExtras = true;
    #   extraPackages = hp: [ hp.xmonad hp.xmonad-contrib hp.xmonad-extras hp.xmonad-config hp.xmobar-config ];
    # };
  };
}
