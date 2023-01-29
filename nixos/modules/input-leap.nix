# Source: nixpkgs/nixos/modules/services/misc/synergy.nix
{ config, lib, pkgs, ... }:

with lib;

let

  cfgC = config.services.input-leap.client;
  cfgS = config.services.input-leap.server;

in

{
  options = {
    services.input-leap = {
      client = {
        enable = mkEnableOption (lib.mdDoc "the Input Leap client (receive keyboard and mouse events from a Input Leap server)");

        screenName = mkOption {
          default = "";
          type = types.str;
          description = lib.mdDoc ''
            Use the given name instead of the hostname to identify
            ourselves to the server.
          '';
        };
        serverAddress = mkOption {
          type = types.str;
          description = lib.mdDoc ''
            The server address is of the form: [hostname][:port].  The
            hostname must be the address or hostname of the server.  The
            port overrides the default port, 24800.
          '';
        };
        autoStart = mkOption {
          default = true;
          type = types.bool;
          description = lib.mdDoc "Whether the Input Leap client should be started automatically.";
        };
      };

      server = {
        enable = mkEnableOption (lib.mdDoc "the Input Leap server (send keyboard and mouse events)");

        configFile = mkOption {
          type = types.path;
          default = "/etc/input-leap-server.conf";
          description = lib.mdDoc "The Input Leap server configuration file.";
        };
        screenName = mkOption {
          type = types.str;
          default = "";
          description = lib.mdDoc ''
            Use the given name instead of the hostname to identify
            this screen in the configuration.
          '';
        };
        address = mkOption {
          type = types.str;
          default = "";
          description = lib.mdDoc "Address on which to listen for clients.";
        };
        autoStart = mkOption {
          default = true;
          type = types.bool;
          description = lib.mdDoc "Whether the Input Leap server should be started automatically.";
        };
      };
    };
  };


  ###### implementation

  config = mkMerge [
    (mkIf (cfgC.enable || cfgS.enable) {
      environment.systemPackages = [ pkgs.input-leap ];
    })

    (mkIf cfgC.enable {
      systemd.user.services.input-leap-client = {
        description = "Input Leap client";

        wantedBy = optional cfgC.autoStart "graphical-session.target";
        after = [ "network.target" "graphical-session.target" ];

        path = [ pkgs.input-leap ];
        restartTriggers = [ cfgC.configFile ];

        serviceConfig = {
          DynamicUser = true;
          ExecStart = ''${pkgs.barrier}/bin/barrierc -f ${optionalString (cfgC.screenName != "") "-n ${cfgC.screenName}"} ${cfgC.serverAddress}'';
          Restart = "always";
        };
      };
    })
    (mkIf cfgS.enable {
      systemd.user.services.input-leap-server = {
        description = "Input Leap server";

        wantedBy = optional cfgS.autoStart "graphical-session.target";
        after = [ "network.target" "graphical-session.target" ];

        path = [ pkgs.input-leap ];
        restartTriggers = [ cfgS.configFile ];

        serviceConfig = {
          DynamicUser = true;
          ExecStart = ''${pkgs.barrier}/bin/barriers -c ${cfgS.configFile} -f${optionalString (cfgS.address != "") " -a ${cfgS.address}"}${optionalString (cfgS.screenName != "") " -n ${cfgS.screenName}"}'';
          Restart = "always";
        };
      };
    })
  ];

}
