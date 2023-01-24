{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.hammerspoon;

  spoonSubmodule = {
    enable = mkOption {
      default = true;
      type = types.bool;
      description = lib.mdDoc ''
        Whether to enable this spoon.
      '';
    };

    name = mkOption {
      type = types.string;
      description = lib.mdDoc ''
        Name of the spoon (as listed on https://www.hammerspoon.org/Spoons/)
      '';
    };
  };

  mkSpoonPackage = spoon: pkgs.stdenv.mkDerivation {
    name = "hammerspoon-${spoon.name}-spoon";
    buildCommand = ''
      mkdir -p $out
      cp -r ${cfg.spoonSources}/Source/${spoon.name}.spoon $out
    '';
  };

  spoonPackages = map mkSpoonPackage cfg.spoons;

  spoonInitWrapper = pkgs.writeText "init.lua" ''
    -- Add spoons in the nix store to the search path
    package.path = package.path .. ";" ..  hs.configdir .. ${lib.concatMapStringsSep " .. \";\" " (spoon: "${spoon}/?.spoon/init.lua") spoonPackages}
  '';
in
{
  # user/501/
  # user/501/org.hammerspoon.Hammerspoon.ServiceProvider
  # user/501/Hammerspoon
  options = {
    services.hammerspoon = {
      enable = mkEnableOption "Enable Hammerspoon";
      daemon.enable = mkEnableOption "Autostart Hammerspoon daemon.";
      configDirectory = mkOption {
        type = types.path;
        description = lib.mdDoc ''
          Hammerspoon configuration directory.
        '';
      };
      spoons = mkOption {
        type = types.listOf (types.submodule {
          options = spoonSubmodule;
        });
        default = pkgs.hammerspoon.passthru.spoons;
        description = lib.mdDoc ''
          Spoons
        '';
      };
      spoonSources = mkOption {
        default = pkgs.hammerspoon.passthru.spoons;
        type = types.package;
        description = lib.mdDoc ''
          Package containing Hammerspoon spoon sources.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.hammerspoon ];


    system.activationScripts.preActivation.text = ''
      mkdir -p ${cfg.configDirectory}
    '';
    system.activationScripts.postActivation.text = ''
      # Link Hammerspoon spoons to the target directory
      echo >&2 "Symlinking Hammerspoon spoons..."
      ln -sf ${spoonInitWrapper} ${cfg.configDirectory}/init.lua
    '';

    launchd.user.agents = {
      hammerspoon-server = lib.mkIf cfg.daemon.enable {
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
