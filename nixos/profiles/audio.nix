{ config
, lib
, pkgs
, ...
}: {
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
  };
  hardware.pulseaudio.enable = false;

  security.rtkit.enable = true;

  environment.systemPackages = with pkgs; [
    easyeffects
    pavucontrol
    alsa-utils
    pulseaudio-ctl

    espeak
  ];
}
