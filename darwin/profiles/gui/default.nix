# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ self', config, lib, pkgs, ... }:
let
  inherit (config.dotfiles.guardian) username;
  inherit (config.home-manager.users.${username}.services.gpg-agent)
    pinentryFlavor;
in
{
  imports = [ ./hammerspoon.nix ./yabai.nix ./xquartz.nix ./safari.nix ./orion.nix ];

  options = { };
  config = {
    environment.systemPackages = with pkgs; [
      gtk-mac-integration
      fontconfig # <- appease 'doom doctor'
      dockutil # <- command line utility for managing macOS dock items
      mysides # <- manages macOS Finder sidebar favorites, tells hilarious jokes

      ferium # CLI program for managing Minecraft mods and modpacks
      ckan # Mod manager for Kerbal Space Program

      self'.packages.better-display # MacOS app for managing display settings
      # darwinPackages.mac-stats # MacOS app for monitoring system stats (fork without telemetry)
      emacsPackages.pdf-tools

      kitty
    ];

    services.karabiner-elements.enable = true;
  };
}
