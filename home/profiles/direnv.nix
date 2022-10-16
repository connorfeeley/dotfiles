{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;

    # Direnv activates in
    enableBashIntegration = false;
  };
  xdg.configFile."direnv/direnvrc".source = "${pkgs.dotfield-config}/direnv/direnvrc";
}
