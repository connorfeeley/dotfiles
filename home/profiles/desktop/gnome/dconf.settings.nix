# Adjusted manually from generated output of dconf2nix
# https://github.com/gvolpe/dconf2nix
{ config, lib, pkgs, ... }:
let
  wallpaper = pkgs.fetchurl {
    # MacOS mojave wallpaper
    url = "https://512pixels.net/downloads/macos-wallpapers-thumbs/10-14-Day-Thumb.jpg";
    sha256 = "01r92v0062c8mbnhi2vya40l6mmhqwa25g23a6qnqzqq4iw78v0v";
  };
in
with lib.hm.gvariant; {
  dconf.enable = !pkgs.stdenv.hostPlatform.isDarwin;
  dconf.settings = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        "pop-shell@system76.com"
      ];
    };
    # "org/gnome/shell/extensions/user-theme" = {
    #   name = config.gtk.theme.name;
    # };
    "org/gnome/desktop/interface" = {
      monospace-font-name = "MesloLGS Nerd Font Mono 10";
      color-scheme = "prefer-dark";
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
      workspaces-only-on-primary = false;
      dynamic-workspaces = false;
    };
    "org/gnome/desktop/wm/preferences" = {
      num-workspaces = 4;
      focus-mode = "sloppy";
    };
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-temperature = "uint32 3500";
      night-light-schedule-automatic = true;
    };
    "org/gnome/eog/ui" = {
      image-gallery = true;
    };
    # Enable and configure pop-shell
    # (see https://github.com/pop-os/shell/blob/master_jammy/scripts/configure.sh)
    "org/gnome/shell/extensions/pop-shell" = {
      active-hint = true;
    };
    "org/gnome/desktop/wm/keybindings" = {
      minimize = [ "<Super>comma" ];
      maximize = [ ];
      unmaximize = [ ];
      switch-to-workspace-left = [ ];
      switch-to-workspace-right = [ ];
      move-to-monitor-up = [ ];
      move-to-monitor-down = [ ];
      move-to-monitor-left = [ ];
      move-to-monitor-right = [ ];
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
      move-to-workspace-1 = [ "<Super><Shift>1" ];
      move-to-workspace-2 = [ "<Super><Shift>2" ];
      move-to-workspace-3 = [ "<Super><Shift>3" ];
      move-to-workspace-4 = [ "<Super><Shift>4" ];
    };
    "org/gnome/shell/keybindings" = {
      open-application-menu = [ ];
      toggle-message-tray = [ "<Super>v" ];
      toggle-overview = [ ];
      switch-to-application-1 = [ ];
      switch-to-application-2 = [ ];
      switch-to-application-3 = [ ];
      switch-to-application-4 = [ ];
      switch-to-application-5 = [ ];
      switch-to-application-6 = [ ];
      switch-to-application-7 = [ ];
      switch-to-application-8 = [ ];
      switch-to-application-9 = [ ];
    };
    "org/gnome/mutter/keybindings" = {
      toggle-tiled-left = [ ];
      toggle-tiled-right = [ ];
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
      ];
      screensaver = "@as ['<Super>Escape']";
      rotate-video-lock-static = [ ];
      home = [ "<Super>e" ];
      email = [ ];
      www = [ ];
      terminal = [ ];
    };
    "org/gnome/mutter/wayland/keybindings" = {
      restore-shortcuts = [ ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Super>z";
      command = "alacritty"; # TODO: use configured "default"
      name = "Open Alacritty";
    };
  };
}
