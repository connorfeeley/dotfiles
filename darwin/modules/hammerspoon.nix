{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.hammerspoon;
in
{
  # user/501/
  # user/501/org.hammerspoon.Hammerspoon.ServiceProvider
  # user/501/Hammerspoon
  options = {
    services.hammerspoon.enable = mkEnableOption "Enable Hammerspoon";

    services.hammerspoon.server = {
      enable = mkEnableOption "Autostart Hammerspoon daemon.";
      configFile = mkOption {
        # TODO
        description = "The IP address or hostname of the server to connect to";
        type = types.path;
        example = "/Users/me/";
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.hammerspoon ];

    launchd.user.agents = {
      hammerspoon-server = lib.mkIf cfg.server.enable {
        serviceConfig = {
          ProgramArguments = [
            # TODO
            "${pkgs.hammerspoon}/Applications/Hammerspoon.app/Contents/MacOS/Hammerspoon"
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
