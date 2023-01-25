{ config
, lib
, pkgs
, ...
}:
{
  # TUI for browsing cheatsheets
  programs.navi = {
    enable = true;
    enableBashIntegration = config.programs.bash.enable;
    enableFishIntegration = config.programs.fish.enable;
    enableZshIntegration = config.programs.zsh.enable;
  };
}
