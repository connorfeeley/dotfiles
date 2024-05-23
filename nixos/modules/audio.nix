# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, pkgs, lib, ... }:
let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.remotePulseAudioServer;
in
{
  options.remotePulseAudioServer = {
    enable = mkEnableOption "Default to a remote pulseaudio server (over TCP).";
    serverAddress = mkOption {
      type = types.str;
      example = lib.literalMD "192.168.0.208";
      description = "Hostname for the virtual machine.";
    };
  };

  config = {
    sound.enable = true;

    services.pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };

    hardware.pulseaudio = {
      enable = false;

      zeroconf = {
        discovery.enable = true;
        publish.enable = true;
      };

      extraClientConf = lib.mkIf cfg.enable ''
        default-server = ${cfg.serverAddress}
      '';
    };

    security.rtkit.enable = true;

    environment.systemPackages = with pkgs; [
      # easyeffects
      pavucontrol
      alsa-utils
      pulseaudio
      pulseaudio-ctl

      espeak
    ];
  };
}
