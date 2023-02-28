{ config, pkgs, ... }: {
  programs = {
    gnome-terminal.enable = true;
    gnome-disks.enable = true;
    kdeconnect.enable = true;
    kdeconnect.package = pkgs.gnomeExtensions.gsconnect;
    seahorse.enable =
      true; # Provides Seahorse, a Gnome application for managing keys and passwords
  };

  services.gnome = {
    # Required for Firefox integration in home-manager
    gnome-browser-connector.enable = true;
    sushi.enable = true; # Quick previewer for Nautilus
    at-spi2-core.enable =
      true; # Service for the Assistive Technologies available on the GNOME platform
    core-developer-tools.enable = true;
    core-os-services.enable = true;
    core-shell.enable = true;
    core-utilities.enable = true;
    evolution-data-server.enable = true;
    games.enable = true;
    glib-networking.enable = true;
    gnome-initial-setup.enable = true;
    gnome-keyring.enable = true;
    gnome-online-accounts.enable = true;
    gnome-online-miners.enable = true;
    gnome-remote-desktop.enable = true;
    gnome-settings-daemon.enable = true;
    gnome-user-share.enable = true;
    rygel.enable = true;
    tracker.enable =
      true; # Search engine, search tool and metadata storage system.
    tracker-miners.enable =
      true; # Tracker miners, indexing services for Tracker search engine and metadata storage system.
  };
  services.gvfs.enable = true; # Virtual filesystem for Gnome
  programs.gnupg.agent.pinentryFlavor = "gnome3";

  environment.systemPackages = with pkgs;
    [ xmonad-config gnome.gnome-boxes ] ++ [
      appmenu-gtk3-module
      tensorman
      popsicle
      firmware-manager

      pop-icon-theme
      marwaita
      marwaita-pop_os
      pop-gtk-theme
      toronto-backgrounds
    ] ++ (with gnomeExtensions; [
      pop-shell
      pop-theme-switcher
      remove-alttab-delay-v2
      systemd-manager
      brightness-control-using-ddcutil

      dash-to-dock
      # dash-to-dock-animator
      blur-my-shell
      topiconsfix # Show tray icons in top bar
      wallpaper-changer-continued

      toronto-backgrounds
    ]);

  services.xserver = {
    enable = true;

    # displayManager.job.preStart = "sleep 5";
    displayManager.gdm = { autoSuspend = false; };

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;

    desktopManager.gnome = {
      enable = true;
      debug = true;
      flashback.enableMetacity = true;

      flashback.customSessions =
        let
          localPath =
            "/home/cfeeley/source/xmonad-config/dist-newstyle/build/x86_64-linux/ghc-9.0.2/xmonad-config-0.1/x/xmonad/build/xmonad/xmonad";
          defaultPath = "${pkgs.xmonad-config}/bin/xmonad";
        in
        [
          {
            wmName = "xmonad-flashback";
            wmLabel = "XMonad flashback";
            wmCommand = localPath;
            enableGnomePanel = true;
          }
          (rec {
            wmCommand = toString (pkgs.writeShellScript "xmonad-flashback" ''
              if [ -n "$DESKTOP_AUTOSTART_ID" ]; then
                  ${pkgs.dbus.out}/bin/dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.RegisterClient "string:${wmLabel}" "string:$DESKTOP_AUTOSTART_ID"
              fi

              ${localPath} &
              waitPID=$!

              if [ -n "$DESKTOP_AUTOSTART_ID" ]; then
                ${pkgs.dbus.out}/bin/dbus-send --print-reply --session --dest=org.gnome.SessionManager "/org/gnome/SessionManager" org.gnome.SessionManager.Logout "uint32:1"
              fi
            '');
            wmLabel = "xmonad-flashback";
            wmName = "XMonad-flashback-DBus";
            enableGnomePanel = true;
          })
        ];
      # sessionPath =
      #   let
      #     fildem-global-menu = pkgs.gnomeExtensions.fildem-global-menu.overrideAttrs (old: {
      #       installPhase =
      #         let
      #           metadata = ''
      #             {
      #               "_generated": "Generated by SweetTooth, do not edit",
      #               "description": "Global menu for Gnome",
      #               "name": "Fildem global menu",
      #               "settings-schema": "org.gnome.shell.extensions.fildem-global-menu",
      #               "shell-version": [
      #                 "3.36",
      #                 "3.38",
      #                 "40",
      #                 "41",
      #                 "42",
      #                 "43"
      #               ],
      #               "url": "https://github.com/gonzaarcr/Fildem",
      #               "uuid": "fildemGMenu@gonza.com",
      #               "version": 2
      #             }
      #           '';
      #         in
      #         old.installPhase + ''echo '${metadata}' > $out/share/gnome-shell/extensions/fildemGMenu@gonza.com/metadata.json'';
      #       dontBuild = false;
      #     });
      #   in
      #   [ pkgs.appmenu-gtk3-module fildem-global-menu pkgs.gnomeExtensions.pop-shell ];

      # sessionPath =
      #   let
      #     gnome-flashback-xmonad = pkgs.callPackage
      #       ({ stdenv, gnome3, bash, haskellPackages, glib, wrapGAppsHook, xmonad-config }: stdenv.mkDerivation {
      #         name = "gnome-flashback-xmonad";

      #         buildInputs = [ gnome3.gnome-flashback gnome3.gnome-panel bash haskellPackages.xmonad glib ];
      #         nativeBuildInputs = [ wrapGAppsHook ];

      #         unpackPhase = "true";

      #         installPhase = ''
      #           mkdir -p $out/libexec
      #           cat << EOF > $out/libexec/gnome-flashback-xmonad
      #           #!${bash}/bin/sh
      #           if [ -z \$XDG_CURRENT_DESKTOP ]; then
      #             export XDG_CURRENT_DESKTOP="GNOME-Flashback:GNOME"
      #           fi
      #           exec ${gnome3.gnome-session}/bin/gnome-session --session=gnome-flashback-xmonad --disable-acceleration-check "\$@"
      #           EOF
      #           chmod +x $out/libexec/gnome-flashback-xmonad
      #           mkdir -p $out/share/gnome-session/sessions
      #           cat << 'EOF' > $out/share/gnome-session/sessions/gnome-flashback-xmonad.session
      #           [GNOME Session]
      #           Name=GNOME Flashback (XMonad)
      #           RequiredComponents=xmonad;gnome-flashback-init;gnome-flashback;gnome-panel;org.gnome.SettingsDaemon.A11ySettings;org.gnome.SettingsDaemon.Clipboard;org.gnome.SettingsDaemon.Color;org.gnome.SettingsDaemon.Datetime;org.gnome.SettingsDaemon.Housekeeping;org.gnome.SettingsDaemon.Keyboard;org.gnome.SettingsDaemon.MediaKeys;org.gnome.SettingsDaemon.Mouse;org.gnome.SettingsDaemon.Power;org.gnome.SettingsDaemon.PrintNotifications;org.gnome.SettingsDaemon.Rfkill;org.gnome.SettingsDaemon.ScreensaverProxy;org.gnome.SettingsDaemon.Sharing;org.gnome.SettingsDaemon.Smartcard;org.gnome.SettingsDaemon.Sound;org.gnome.SettingsDaemon.Wacom;org.gnome.SettingsDaemon.XSettings;
      #           EOF
      #           mkdir -p $out/share/applications
      #           cat << 'EOF' > $out/share/applications/xmonad.desktop
      #           [Desktop Entry]
      #           Type=Application
      #           Encoding=UTF-8
      #           Name=Xmonad
      #           Exec=${xmonad-config}/bin/xmonad
      #           NoDisplay=true
      #           X-GNOME-WMName=Xmonad
      #           X-GNOME-Autostart-Phase=WindowManager
      #           X-GNOME-Provides=windowmanager
      #           X-GNOME-Autostart-Notify=false
      #           EOF
      #           mkdir -p $out/share/xsessions
      #           cat << EOF > $out/share/xsessions/gnome-flashback-xmonad.desktop
      #           [Desktop Entry]
      #           Name=GNOME Flashback (XMonad)
      #           Comment=This session logs you into GNOME Flashback with XMonad
      #           Exec=$out/libexec/gnome-flashback-xmonad
      #           TryExec=${xmonad-config}/bin/xmonad
      #           Type=Application
      #           DesktopNames=GNOME-Flashback;GNOME;
      #           EOF
      #         '';
      #       })
      #       { };
      #   in
      #   [ gnome-flashback-xmonad ];
    };
  };

}
