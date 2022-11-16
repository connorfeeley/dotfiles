{ config
, lib
, pkgs
, ...
}: {
  imports = [
    ./hammerspoon.nix
    ./yabai.nix
  ];

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
    "FelixKratz/formulae"
    "homebrew/cask"
    "homebrew/cask-versions"
    "railwaycat/emacsmacport"
    "jorgelbg/tap"
  ];

  homebrew.brews = [
    # This provides a GUI, despite it not being a cask.
    # NOTE: now using 'pinentry_mac' package
    "pinentry-mac"

    # Interesting project; however the password is not stored in the
    # secure enclave, which is a dealbreaker for me 😕.
    # "pinentry-touchid"

    "freerdp"

    "alerter" # like notify-send but for darwin
  ];

  homebrew.casks = [
    "emacs-mac"
    "visual-studio-code"

    "utm-beta" # QEMU GUI with virtualisation support
    "barrier" # Poor man's KVM
    "swiftbar" # Tweak the menu bar
    "hiddenbar" # Hide/toggle menu bar icon visibility
    "rectangle" # Resize windows with keyboard shortcuts and snap areas
    "raycast" # Better search popup

    # "docker"

    "karabiner-elements"

    "firefox"
    "google-chrome"
    "microsoft-edge"

    "microsoft-teams"
    "microsoft-outlook"

    "nordvpn"
    "private-internet-access"

    "vlc"
    "xquartz"
    "lulu"
    "stats"
    "steam"
    "curseforge"
    "minecraft"
    "feed-the-beast"
    "spotify"
    "deluge"
    "discord"
    "balenaetcher"
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
}
