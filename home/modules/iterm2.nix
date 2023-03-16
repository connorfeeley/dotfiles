{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) isDarwin;
  inherit (lib) types mkIf mkEnableOption mkOption;

  shellInit = shell:
    "source " + cfg.package
    + "/Applications/iTerm2.app/Contents/Resources/iterm2_shell_integration."
    + shell;

  cfg = config.programs.iterm2;
in
{
  options.programs.iterm2 = {
    enable =
      mkEnableOption "Enable the iTerm2 terminal emulator (system-wide).";

    package = mkOption {
      type = types.package;
      default =
        if isDarwin then
          pkgs.iterm2
        else
          pkgs.iterm2.overrideAttrs (o: {
            # Remove the generated binary
            installPhase = o.installPhase + ''
              rm -rf $out/bin
            '';
            meta.platforms = o.meta.platforms ++ lib.platforms.linux;
          });
      description = "The iTerm2 package to use.";
    };

    enableBashIntegration = mkOption {
      type = types.bool;
      default = config.programs.bash.enable;
      description = "Enable iTerm2 bash integration.";
    };

    enableZshIntegration = mkOption {
      type = types.bool;
      default = config.programs.zsh.enable;
      description = "Enable iTerm2 zsh integration.";
    };

    #homeManagerModules.nixvim = import ./wrappers/hm.nix modules;
    enableFishIntegration = mkOption {
      type = types.bool;
      default = config.programs.fish.enable;
      description = "Enable iTerm2 fish integration.";
    };
  };

  config = {
    home.packages = mkIf cfg.enable [ cfg.package ];

    programs.bash.initExtra = mkIf cfg.enableBashIntegration (shellInit "bash");

    programs.zsh.initExtraFirst = mkIf cfg.enableZshIntegration (shellInit "zsh");

    programs.fish.shellInit = mkIf cfg.enableFishIntegration (shellInit "fish");
  };
}
