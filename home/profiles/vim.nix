{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [ ];

  # pta2022's neovim flake
  programs.nixvim.enable = true;
}
