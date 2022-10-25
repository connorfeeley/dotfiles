{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./hammerspoon.nix
    ./yabai.nix
  ];

  environment.systemPackages = with pkgs; [
    iterm2 # the king of macOS terminals; not kitty 😾
    gtk-mac-integration
    fontconfig # appease 'doom doctor'
    dockutil # command line utility for managing macOS dock items
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
    # "pinentry-mac"

    # Interesting project; however the password is not stored in the
    # secure enclave, which is a dealbreaker for me 😕.
    # "pinentry-touchid"

    "freerdp"

    "alerter" # like notify-send but for darwin
  ];

  homebrew.casks = [
    "emacs-mac"

    "utm-beta" # QEMU GUI with virtualisation support
    "barrier" # Poor man's KVM
    "swiftbar" # Tweak the menu bar
    "hiddenbar" # Hide/toggle menu bar icon visibility
    "rectangle" # Resize windows with keyboard shortcuts and snap areas
    "raycast" # Better search popup

    "docker"

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
    "spotify"
    "deluge"
  ];

  homebrew.masApps = {
    "Xcode" = 497799835;
    "Keynote" = 409183694;
    "Numbers" = 409203825;
    "Pages" = 409201541;

    "Bitwarden" = 1352778147; # MAS version can integrate w/ FF for biometrics

    "Steam Link" = 1246969117;
  };
}
