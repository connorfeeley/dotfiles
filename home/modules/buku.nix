{ config, lib, pkgs, ... }:
let

  cfg = config.programs.buku;
in
{
  options = {
    programs.buku = {
      enable = lib.mkEnableOption "Whether to enable the module for the Buku bookmaking tool.";
      enableTui = lib.mkEnableOption "Whether to enable 'bukut' TUI.";
      enableBrowserIntegration = lib.mkEnableOption "Whether to enable the Bukubrow native messaging host for browsers.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.buku ];
    warnings = lib.optional cfg.enableBrowserIntegration
      "programs.buku.enableBrowserIntegration: bukubrow was removed from nixpkgs 26.05; browser integration is currently a no-op.";
  };
}
