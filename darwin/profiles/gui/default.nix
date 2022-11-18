{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.dotfield.guardian) username;
  inherit (config.home-manager.users.${username}.services.gpg-agent) pinentryFlavor;
in
{
  imports = [
    ./hammerspoon.nix
    ./yabai.nix
    ./xquartz.nix
  ];

  options = { };
  config = {
    environment.systemPackages = with pkgs; [
      gtk-mac-integration
      iterm2 #     <- the king of macOS terminals; not kitty 😾
      fontconfig # <- appease 'doom doctor'
      dockutil #   <- command line utility for managing macOS dock items
      mysides #    <- manages macOS Finder sidebar favorites, tells hilarious jokes

      ferium # CLI program for managing Minecraft mods and modpacks
    ];

    environment.variables = {
      DOOMDIR = "$XDG_CONFIG_HOME/doom";
      EMACSDIR = "$XDG_CONFIG_HOME/emacs";
    };

    homebrew.taps = [
      { name = "FelixKratz/formulae"; }
      { name = "homebrew/cask"; }
      { name = "homebrew/cask-versions"; }
      { name = "railwaycat/emacsmacport"; }
      { name = "jorgelbg/tap"; }
    ];

    homebrew.brews = [
      (lib.mkIf (pinentryFlavor == "mac" || pinentryFlavor == "touchid") {
        # 'pinentry-mac' or 'pinentry-touchid' ('-touchid' is exclusive with '-mac')
        # Both provide a GUI, despite neither being a cask.
        name = "pinentry-${pinentryFlavor}";

        # First time setup:
        # defaults write org.gpgtools.common UseKeychain -bool yes
        # echo 1234 | gpg -as -
        # (check 'Save in Keychain' box in prompt)
        # (Select 'Always Allow' if prompted)
      })

      { name = "freerdp"; }

      { name = "alerter"; } # like notify-send but for darwin
    ];

    homebrew.casks = [
      { name = "emacs-mac"; }
      { name = "visual-studio-code"; }

      { name = "utm-beta"; } # QEMU GUI with virtualisation support
      { name = "barrier"; } # Poor man's KVM
      { name = "swiftbar"; } # Tweak the menu bar
      { name = "hiddenbar"; } # Hide/toggle menu bar icon visibility
      { name = "rectangle"; } # Resize windows with keyboard shortcuts and snap areas
      { name = "raycast"; } # Better search popup

      # { name = "docker"; }

      { name = "karabiner-elements"; }

      { name = "firefox"; }
      { name = "google-chrome"; }
      { name = "microsoft-edge"; }

      { name = "microsoft-teams"; }
      { name = "microsoft-outlook"; }

      { name = "nordvpn"; }
      { name = "private-internet-access"; }

      { name = "vlc"; }
      { name = "lulu"; }
      { name = "stats"; }
      { name = "steam"; }
      { name = "curseforge"; }
      { name = "minecraft"; }
      { name = "feed-the-beast"; }
      { name = "spotify"; }
      { name = "deluge"; }
      { name = "discord"; }
      { name = "balenaetcher"; }
    ];

    homebrew.masApps = {
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
