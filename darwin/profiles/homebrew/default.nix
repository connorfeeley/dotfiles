# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let
  inherit (config.dotfiles.guardian) username;
  inherit (config.home-manager.users.${username}.services.gpg-agent) pinentryFlavor;
in
{
  config = {
    homebrew.taps = [
      { name = "FelixKratz/formulae"; }
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
      # Broken
      # { name = "alerter"; } # like notify-send but for darwin
      { name = "postgresql@16"; }
      { name = "wordnet"; } # for doom emacs 'lookup'
      # { name = "emacs-plus@29"; args = [ "with-dbus" "with-xwidgets"  "with-native-comp" "with-poll" ]; link = true; } # emacs-plus
      { name = "emacs-mac"; args = [ "with-starter" "with-librsvg" "with-dbus" "with-mac-metal" "with-xwidgets" "with-native-comp" ]; link = true; } # macport
      { name = "ghcup"; }
    ];

    homebrew.casks = [
      { name = "visual-studio-code"; }
      { name = "utm@beta"; } # QEMU GUI with virtualisation support
      # { name = "barrier"; } # Poor man's KVM
      { name = "swiftbar"; } # Tweak the menu bar
      # { name = "hiddenbar"; } # Hide/toggle menu bar icon visibility
      { name = "rectangle"; } # Resize windows with keyboard shortcuts and snap areas
      { name = "raycast"; } # Better search popup
      # { name = "bluesnooze"; } # Sleeping Mac = Bluetooth off
      { name = "docker"; }
      { name = "firefox"; }
      { name = "microsoft-teams"; }
      { name = "microsoft-outlook"; }
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
      { name = "qflipper"; } # flipper zero
      { name = "cirrus"; } # iCloud debugger
      { name = "element"; } # Matrix client

      # LibreOffice
      { name = "libreoffice"; }
      { name = "libreoffice-language-pack"; }

      { name = "whisky"; } # Games on MacOS

      { name = "notunes"; } # Prevent Apple Music from launching

      { name = "bartender"; }

      { name = "krita"; } # Image editor that isn't GIMP
    ];

    homebrew.masApps = {
      "Developer" = 640199958;
      "Xcode" = 497799835;

      "Keynote" = 409183694;
      "Numbers" = 409203825;
      "Pages" = 409201541;

      "Bitwarden" = 1352778147; # MAS version can integrate w/ FF for biometrics

      "Steam Link" = 1246969117;

      "Apple Configurator" = 1037126344;
      "Apple Developer" = 640199958;

      "Microsoft Remote Desktop" = 1295203466;

      # Live broadcasts of the legislature of the province of Ontario, Canada.
      # NOTE: This is an iPad app, which 'mas' doesn't seem to handle correctly.
      # "Parlance" = 1520014900;
    };
  };
}
