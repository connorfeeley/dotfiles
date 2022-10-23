{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.lib) mkOption;
  inherit (pkgs.lib.types) int str;

  cfg = config.theme;
  normalWeight = 400;
in
{
  options = {
    theme = {
      enable = lib.mkEnableOption "Whether to enable the theme module.";
      font = {
        mono = {
          # FIXME: set a sensible default
          family = mkOption { type = str; default = ""; };
          weight = mkOption { type = int; default = normalWeight; };
          size = mkOption { type = int; default = 13; };
        };
        sans = {
          # FIXME: set a sensible default
          family = mkOption { type = str; default = ""; };
          weight = mkOption { type = int; default = normalWeight; };
          size = mkOption { type = int; default = 10; };
        };
        serif = {
          # FIXME: set a sensible default
          family = mkOption { type = str; default = ""; };
          weight = mkOption { type = int; default = normalWeight; };
          size = mkOption { type = int; default = cfg.font.sans.size; };
        };
        # FIXME: ?
        emoji.family = mkOption { type = str; default = ""; };
      };
    };
  };
}
