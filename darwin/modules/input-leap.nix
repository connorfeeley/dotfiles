{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.input-leap;
in

{
  options = {
    services.input-leap.enable = mkEnableOption "Enable Input Leap to share keyboard and mouse between computers";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.input-leap ];

    # system.activationScripts.extraActivation.text = ''
    #   rm -rf ${parentAppDir}
    #   mkdir -p ${parentAppDir}
    #   # Kernel extensions must reside inside of /Applications, they cannot be symlinks
    #   cp -r ${pkgs.input-leap.driver}/Applications/.Karabiner-VirtualHIDDevice-Manager.app ${parentAppDir}
    # '';

    launchd.daemons.karabiner_grabber = {
      serviceConfig.ProgramArguments = [
        "${pkgs.input-leap}/Applications/Barrier.app/MacOS/barrier"
      ];
      serviceConfig.ProcessType = "Interactive";
      serviceConfig.Label = "org.pqrs.karabiner.karabiner_grabber";
      serviceConfig.KeepAlive.SuccessfulExit = true;
      serviceConfig.KeepAlive.Crashed = true;
      serviceConfig.KeepAlive.AfterInitialDemand = true;
    };

    # environment.userLaunchAgents."org.pqrs.karabiner.agent.karabiner_grabber.plist".source = "${pkgs.input-leap}/Library/LaunchAgents/org.pqrs.karabiner.agent.karabiner_grabber.plist";
  };
}
