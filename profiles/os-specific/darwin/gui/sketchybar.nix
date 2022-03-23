{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
in {
  homebrew.brews = ["sketchybar"];

  services.yabai.config.external_bar = "main:32:0";

  # The system menu bar must be hidden when using a custom bar.
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  my.hm.xdg.configFile."sketchybar".source = "${config.dotfield.configDir}/sketchybar";
}
