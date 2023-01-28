{ config, lib, pkgs, ... }:
{
  homebrew.brews = [
    { name = "pulseaudio"; }
  ];
}
