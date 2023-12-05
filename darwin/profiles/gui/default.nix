# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self', config, lib, pkgs, ... }:
let
  inherit (config.dotfield.guardian) username;
  inherit (config.home-manager.users.${username}.services.gpg-agent)
    pinentryFlavor;
in
{
  imports =
    [ ./hammerspoon.nix ./yabai.nix ./xquartz.nix ./safari.nix ./orion.nix ];

  options = { };
  config = {
    environment.systemPackages = with pkgs; [
      gtk-mac-integration
      fontconfig # <- appease 'doom doctor'
      dockutil # <- command line utility for managing macOS dock items
      mysides # <- manages macOS Finder sidebar favorites, tells hilarious jokes
      dbeaver # <- GUI database manager

      ferium # CLI program for managing Minecraft mods and modpacks
      ckan # Mod manager for Kerbal Space Program

      self'.packages.better-display # MacOS app for managing display settings
      # darwinPackages.mac-stats # MacOS app for monitoring system stats (fork without telemetry)
      emacsPackages.pdf-tools

      kitty
    ];

    services.karabiner-elements.enable = true;

    homebrew.taps = [
      { name = "FelixKratz/formulae"; }
      { name = "homebrew/cask-versions"; }
      { name = "jorgelbg/tap"; } # pinentry-touchid
      { name = "homebrew/services"; } # 'brew services' command
      { name = "apple/homebrew-apple"; } # game porting toolkit
      { name = "d12frosted/emacs-plus"; }
      { name = "railwaycat/emacsmacport"; }
    ];

    homebrew.brews = [
      (lib.mkIf (pinentryFlavor == "mac" || pinentryFlavor == "touchid") {
        # 'pinentry-mac' or 'pinentry-touchid' ('-touchid' is exclusive with '-mac')
        # Both provide a GUI, despite neither being a cask.
        name = "pinentry-${pinentryFlavor}";

        # First time setup (pinentry-mac):
        # defaults write org.gpgtools.common UseKeychain -bool yes
        # gpgconf --kill gpg-agent
        # echo 1234 | gpg -as -
        # (check 'Save in Keychain' box in prompt)
        # (Select 'Always Allow' if prompted)
        #
        # If using pinentry-touchid, first set pinentryFlavor to "mac", switch system config,
        # follow pinentry-mac instructions, set pinentryFlavor to "touchid", switch system config,
        # then run:
        # gpgconf --kill gpg-agent
        # defaults write org.gpgtools.common DisableKeychain -bool yes
      })

      { name = "samba"; }
      { name = "freerdp"; }
      { name = "alerter"; } # like notify-send but for darwin
      { name = "postgresql@15"; }
      { name = "wordnet"; } # for doom emacs 'lookup'
      # { name = "emacs-plus@29"; args = [ "with-dbus" "with-xwidgets"  "with-native-comp" "with-poll" ]; link = true; } # emacs-plus
      { name = "emacs-mac"; args = [ "with-starter" "with-librsvg" "with-dbus" "with-mac-metal" "with-xwidgets"  "with-native-comp" ]; link = true; } # macport
    ];

    homebrew.casks = [
      { name = "visual-studio-code"; }
      { name = "utm-beta"; } # QEMU GUI with virtualisation support
      # { name = "barrier"; } # Poor man's KVM
      { name = "swiftbar"; } # Tweak the menu bar
      # { name = "hiddenbar"; } # Hide/toggle menu bar icon visibility
      { name = "rectangle"; } # Resize windows with keyboard shortcuts and snap areas
      { name = "raycast"; } # Better search popup
      # { name = "bluesnooze"; } # Sleeping Mac = Bluetooth off
      { name = "docker"; }
      { name = "firefox"; }
      { name = "google-chrome"; }
      { name = "microsoft-edge"; }
      { name = "microsoft-teams"; }
      { name = "microsoft-outlook"; }
      { name = "nordvpn"; }
      { name = "private-internet-access"; }
      { name = "vlc"; }
      { name = "lulu"; }
      { name = "wireshark"; }
      { name = "steam"; }
      { name = "curseforge"; }
      { name = "minecraft"; }
      { name = "feed-the-beast"; }
      { name = "spotify"; }
      { name = "deluge"; }
      { name = "discord"; }
      { name = "balenaetcher"; }
      { name = "gimp"; }
      { name = "zoom"; }
      { name = "calibre"; }
      { name = "sloth"; }
      { name = "talon"; }
      { name = "handbrake"; }
      { name = "transmission"; }
      { name = "deluge"; }
      { name = "nomachine"; }
      { name = "db-browser-for-sqlite"; }
      { name = "launchcontrol"; }
      { name = "backuploupe"; }
      { name = "timemachineeditor"; } # start backups in Time Machine at particular times
      { name = "disk-inventory-x"; } # disk usage analyzer
      { name = "trader-workstation"; } # Interactive Brokers TWS
      { name = "microsoft-remote-desktop"; }
      { name = "pgadmin4"; }
      { name = "obs"; }
      { name = "stats"; }
      { name = "x2goclient"; }
      { name = "anki"; }
      { name = "teamviewer"; }
      { name = "arc"; } # browser
    ];

    homebrew.masApps = {
      "Developer" = 640199958;
      "Xcode" = 497799835;

      "Keynote" = 409183694;
      "Numbers" = 409203825;
      "Pages" = 409201541;

      "Bitwarden" = 1352778147; # MAS version can integrate w/ FF for biometrics

      "Steam Link" = 1246969117;

      # Live broadcasts of the legislature of the province of Ontario, Canada.
      # Civic engagement is good, mmkay?
      # FIXME: this is an iPad app, which 'mas' doesn't seem to handle correctly.
      # "Parlance" = 1520014900;
    };
  };
}
