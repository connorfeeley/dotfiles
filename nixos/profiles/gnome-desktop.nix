{ config
, pkgs
, ...
}: {
  services.gnome = {
    # Required for Firefox integration in home-manager
    gnome-browser-connector.enable = true;
    sushi.enable = true; # Quick previewer for Nautilus
    at-spi2-core.enable = true; # Service for the Assistive Technologies available on the GNOME platform
    core-developer-tools.enable = true;
    core-os-services.enable = true;
    core-shell.enable = true;
    core-utilities.enable = true;
    evolution-data-server.enable = true;
    games.enable = true;
    glib-networking.enable = true;
    gnome-initial-setup.enable = false;
    gnome-keyring.enable = true;
    gnome-online-accounts.enable = true;
    gnome-online-miners.enable = true;
    gnome-remote-desktop.enable = true;
    gnome-settings-daemon.enable = true;
    gnome-user-share.enable = true;
    rygel.enable = true;
    tracker.enable = true; # Search engine, search tool and metadata storage system.
    tracker-miners.enable = true; # Tracker miners, indexing services for Tracker search engine and metadata storage system.
  };
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
