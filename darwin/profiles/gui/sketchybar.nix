{ config
, lib
, pkgs
, inputs
, ...
}:
{
  homebrew.brews = [{ name = "sketchybar"; }];

  services.yabai.config.external_bar = "main:32:0";

  # The system menu bar must be hidden when using a custom bar.
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
}
