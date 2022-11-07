{ config
, lib
, pkgs
, ...
}: {
  environment.systemPackages = with pkgs; [
    mpv
    vlc
  ];
}
