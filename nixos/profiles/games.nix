{ config, pkgs, lib, ... }:
lib.mkIf (!config.nixos-vm.enable) {
  environment.systemPackages = with pkgs; [
    discord

    minecraft
    ferium # CLI program for managing Minecraft modpacks from Modrinth, CurseForge, and Github Releases

    ckan # Mod manager for Kerbal Space Program
  ];

  programs.steam.enable = true;
}
