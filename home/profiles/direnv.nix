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
    config = {
      global.warn_timeout = "10s"; # Wait 10s before warning that about long-running instances
      whitelist = {
        # Implicitly allow any direnv configurations under these paths
        prefix= [
          "$HOME/dev"
          "$XDG_CONFIG_HOME"
        ];
      };
    };
  };

  # Prevent shells from being garbage-collected
  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
  };

  xdg.configFile."direnv/direnvrc".source = "${pkgs.dotfield-config}/direnv/direnvrc";
}
