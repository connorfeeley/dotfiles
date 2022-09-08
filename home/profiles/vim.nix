{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # Wil Taylor's neovim configuration
    wiltaylor-neovim
  ];
}
