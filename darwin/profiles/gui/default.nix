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
    gtk-mac-integration

    # Appease 'doom doctor'
    fontconfig
  ];

  homebrew.taps = [
    "FelixKratz/formulae"
    "homebrew/cask"
    "homebrew/cask-versions"
  ];

  homebrew.brews = [
    # This provides a GUI, despite it not being a cask.
    "pinentry-mac"

    # Interesting project; however the password is not stored in the
    # secure enclave, which is a dealbreaker for me 😕.
    # "pinentry-touchid"

    "freerdp"

    "alerter" # like notify-send but for darwin
  ];

  homebrew.casks = [
    "utm-beta" # QEMU GUI with virtualisation support
    "barrier" # Poor man's KVM
    "swiftbar" # Tweak the menu bar
    "hiddenbar" # Hide/toggle menu bar icon visibility
    "rectangle" # Resize windows with keyboard shortcuts and snap areas

    "docker"

    "karabiner-elements"

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
    "Bitwarden" = 1352778147; # MAS version can integrate w/ FF for biometrics
  };
}
