{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.input-leap;
in

{
  options = {
    services.input-leap.enable = mkEnableOption "Enable Input Leap to share keyboard and mouse between computers";

    services.input-leap.client = {
      enable = mkEnableOption "Autostart Input Leap client daemon.";
      serverAddress = mkOption {
        type = types.str;
        description = lib.mdDoc ''
          The server address is of the form: [hostname][:port].  The
          hostname must be the address or hostname of the server.  The
          port overrides the default port, 24800.
        '';
      };
    };

    services.input-leap.server = {
      enable = mkEnableOption "Autostart Input Leap server daemon.";
      configFile = mkOption {
        description = "The IP address or hostname of the server to connect to";
        type = types.path;
        example = "/Users/me/";
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.input-leap ];

    launchd.user.agents = {
      input-leap-client = lib.mkIf cfg.client.enable {
        serviceConfig = {
          ProgramArguments = [
            "${pkgs.input-leap}/Applications/Barrier.app/Contents/MacOS/barrierc"
            "--no-daemon"
            cfg.client.serverAddress
          ];
          Label = "org.debauchee.com.barrierc";
          OnDemand = false;
          RunAtLoad = true;
        };
      };
      input-leap-server = lib.mkIf cfg.server.enable {
        serviceConfig = {
          ProgramArguments = [
            "${pkgs.input-leap}/Applications/Barrier.app/Contents/MacOS/barriers"
            "--no-daemon"
            "--config"
            cfg.server.configFile
          ];
          Label = "org.debauchee.com.barriers";
          OnDemand = false;
          RunAtLoad = true;
        };
      };
    };
  };
}
