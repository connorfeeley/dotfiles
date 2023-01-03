{ config
, pkgs
, ...
}: {
  # Required for Firefox integration in home-manager
  services.gnome.gnome-browser-connector.enable = true;

  services.gnome.sushi.enable = true;
  # programs.gnupg.agent.pinentryFlavor = "gnome3";

  services.xserver = {
    enable = true;

    displayManager.gdm.enable = true;

    desktopManager.gnome = {
      enable = true;
      extraGSettingsOverrides = ''
        [org.gnome.desktop.input-sources]
        sources=[('xkb', '${config.services.xserver.layout}')]
        xkb-options=['${config.services.xserver.xkbOptions}']
      '';
    };

    desktopManager.gnome3 = {
      enable = true;
      flashback.enableMetacity = true;
      flashback.customSessions = [
        {
          wmCommand = "${pkgs.xmonad-config}/bin/xmonad";
          wmLabel = "XMonad";
          wmName = "xmonad";
          enableGnomePanel = true;
        }
      ];
    };
  };
}
