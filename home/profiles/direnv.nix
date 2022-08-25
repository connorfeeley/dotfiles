{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;

    enableBashIntegration = true;
  };
  xdg.configFile."direnv/direnvrc".source = "${pkgs.dotfield-config}/direnv/direnvrc";
}
