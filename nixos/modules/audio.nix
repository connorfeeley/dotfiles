# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, lib, ... }: {
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
    extraClientConf = ''
      default-server = 192.168.0.208
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
}
