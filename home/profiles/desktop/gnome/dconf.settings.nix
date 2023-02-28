# Adjusted manually from generated output of dconf2nix
# https://github.com/gvolpe/dconf2nix
{ config, lib, pkgs, ... }:
let
  wallpaper = pkgs.fetchurl {
    # MacOS mojave wallpaper
    url =
      "https://512pixels.net/downloads/macos-wallpapers-thumbs/10-14-Day-Thumb.jpg";
    sha256 = "01r92v0062c8mbnhi2vya40l6mmhqwa25g23a6qnqzqq4iw78v0v";
  };
in
with lib.hm.gvariant; {
  dconf.enable = !pkgs.stdenv.hostPlatform.isDarwin;
  dconf.settings = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "apps-menu@gnome-shell-extensions.gcampax.github.com"
        "auto-move-windows@gnome-shell-extensions.gcampax.github.com"
        "gsconnect@andyholmes.github.io"
        "launch-new-instance@gnome-shell-extensions.gcampax.github.com"
        "native-window-placement@gnome-shell-extensions.gcampax.github.com"
        "places-menu@gnome-shell-extensions.gcampax.github.com"
        "pop-launcher-super-key@ManeLippert"
        "pop-shell@system76.com"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
        "screenshot-window-sizer@gnome-shell-extensions.gcampax.github.com"
        "systemd-manager@hardpixel.eu"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "window-list@gnome-shell-extensions.gcampax.github.com"
        "windowsNavigator@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
        "remove-alt-tab-delay@daase.net"
        "dash-to-dock@micxgx.gmail.com"
        "blur-my-shell@aunetx"
        "topiconsfix@aleskva@devnullmail.com"
        "display-brightness-ddcutil@themightydeity.github.com"
      ];
    };
    # "org/gnome/shell/extensions/user-theme" = {
    #   name = config.gtk.theme.name;
    # };

    "org/gnome/desktop/input-sources" = {
      per-window = false;
      xkb-options = [ ];
      sources = [ (mkTuple [ "xkb" "us+dvorak" ]) ];
    };

    "org/gnome/desktop/interface" = {
      monospace-font-name = "Iosevka 10";
      color-scheme = "prefer-light";
    };
    "org/gnome/desktop/background" = {
      primary-color = "#000000";
      secondary-color = "#000000";
      picture-uri = "file://${wallpaper}";
      picture-uri-dark = "file://${wallpaper}";
    };
    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };
    "org/gnome/mutter" = {
      edge-tiling = true;
      workspaces-only-on-primary = true;
      dynamic-workspaces = false;
      experminental-features =
        [ "x11-randr-fractional-scaling" ]; # Enable fractional scaling
    };
    "org/gnome/desktop/wm/preferences" = {
      num-workspaces = 9;
      focus-mode = "sloppy";
    };
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-temperature = "uint32 3700";
      night-light-schedule-automatic = true;
    };
    "org/gnome/eog/ui" = { image-gallery = true; };
    # Enable and configure pop-shell
    # (see https://github.com/pop-os/shell/blob/master_jammy/scripts/configure.sh)
    "org/gnome/shell/extensions/pop-shell" = { active-hint = true; };
    "org/gnome/desktop/wm/keybindings" = {
      minimize = [ "<Super>comma" ];
      maximize = [ "<Super>f" ];
      unmaximize = [ ];
      switch-to-workspace-left = [ "<Super>e" ];
      switch-to-workspace-right = [ "<Super>r" ];
      move-to-monitor-up = [ ];
      move-to-monitor-down = [ ];
      move-to-monitor-left = [ "<Super><Shift>e" ];
      move-to-monitor-right = [ "<Super><Shift>r" ];
      move-to-workspace-down = [ ];
      move-to-workspace-up = [ ];
      switch-to-workspace-down = [ "<Primary><Super>Down" "<Primary><Super>j" ];
      switch-to-workspace-up = [ "<Primary><Super>Up" "<Primary><Super>k" ];
      toggle-maximized = [ "<Super>f" ];
      close = [ "<Super>q" "<Alt>F4" ];
      switch-to-workspace-1 = [ "<Super>1" ];
      switch-to-workspace-2 = [ "<Super>2" ];
      switch-to-workspace-3 = [ "<Super>3" ];
      switch-to-workspace-4 = [ "<Super>4" ];
      switch-to-workspace-5 = [ "<Super>5" ];
      switch-to-workspace-6 = [ "<Super>6" ];
      switch-to-workspace-7 = [ "<Super>7" ];
      switch-to-workspace-8 = [ "<Super>8" ];
      switch-to-workspace-9 = [ "<Super>9" ];
      move-to-workspace-1 = [ "<Super><Shift>1" ];
      move-to-workspace-2 = [ "<Super><Shift>2" ];
      move-to-workspace-3 = [ "<Super><Shift>3" ];
      move-to-workspace-4 = [ "<Super><Shift>4" ];
      move-to-workspace-5 = [ "<Super><Shift>5" ];
      move-to-workspace-6 = [ "<Super><Shift>6" ];
      move-to-workspace-7 = [ "<Super><Shift>7" ];
      move-to-workspace-8 = [ "<Super><Shift>8" ];
      move-to-workspace-9 = [ "<Super><Shift>9" ];
    };
    "org/gnome/shell/keybindings" = {
      open-application-menu = [ ];
      toggle-message-tray = [ "<Super>v" ];
      toggle-overview = [ ];
      switch-to-application-1 = [ "<Super><Control>1" ];
      switch-to-application-2 = [ "<Super><Control>2" ];
      switch-to-application-3 = [ "<Super><Control>3" ];
      switch-to-application-4 = [ "<Super><Control>4" ];
      switch-to-application-5 = [ "<Super><Control>5" ];
      switch-to-application-6 = [ "<Super><Control>6" ];
      switch-to-application-7 = [ "<Super><Control>7" ];
      switch-to-application-8 = [ "<Super><Control>8" ];
      switch-to-application-9 = [ "<Super><Control>9" ];
    };
    "org/gnome/mutter/keybindings" = {
      switch-monitor = [ ];
      toggle-tiled-left = [ ];
      toggle-tiled-right = [ ];
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
      ];
      screensaver = "@as []";
      rotate-video-lock-static = [ ];
      home = [ ];
      email = [ ];
      www = [ "firefox" ];
      terminal = [ "kitty" ];
    };
    "org/gnome/mutter/wayland/keybindings" = { restore-shortcuts = [ ]; };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" =
      {
        binding = "<Super><Shift>Return";
        command = "kitty";
        name = "Open Kitty";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" =
      {
        binding = "<Super>p";
        command = "rofi -show combi";
        name = "Open Rofi";
      };

    # Stop spamming "The system is going down for suspend NOW!". It isn't.
    "org/gnome/settings-daemon/plugins/power".sleep-inactive-ac-type = "nothing";
  };
}
