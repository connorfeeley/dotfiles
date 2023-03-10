{ config, lib, pkgs, ... }:
let
  inherit (builtins) map;

  cfg = config.programs.fish;

  mkPlugin = name: {
    inherit name;
    inherit (pkgs.sources."fish-${name}") src;
  };
in
{
  options.programs.fish = {
    autopair.enable = lib.mkEnableOption
      "Whether to enable autopairing of symbols with the autopair plugin.";
    fifc.enable = lib.mkEnableOption "Whether to enable the fifc fish plugin.";
    bass.enable = lib.mkEnableOption "Whether to enable the bass fish plugin.";
  };

  config = lib.mkIf cfg.enable {
    lib.fish = { inherit mkPlugin; };

    programs.fish.plugins = map mkPlugin
      ((lib.optional cfg.autopair.enable "autopair")
        ++ (lib.optional cfg.fifc.enable "fifc")
        ++ (lib.optional cfg.bass.enable "bass"));
  };
}
