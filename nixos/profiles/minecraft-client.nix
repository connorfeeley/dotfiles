{ config
, lib
, pkgs
, profiles
, suites
, inputs
, primaryUser
, collective
, ...
}:
let
in
{
  environment.systemPackages = with pkgs; [
    # CLI program for managing Minecraft modpacks from Modrinth, CurseForge, and Github Releases
    ferium
  ];
}
