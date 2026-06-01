{ pkgs, ... }: {
  # FIXME: pkgs.plusultra.neovim provided by neovim-plusultra overlay,
  # which is currently disabled (see flake-modules/overlays.nix).
  # home.packages = [ pkgs.plusultra.neovim ];
  home.packages = [ pkgs.neovim ];
}
