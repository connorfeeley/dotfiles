# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib, pkgs, ... }:
{
  ###
  ### KDE
  ###
  services.desktopManager.plasma6 = {
    enable = true;
  };

  services.xserver.desktopManager.plasma5 = {
    enable = false;
    useQtScaling = true;
    runUsingSystemd = false;
  };

  qt = {
    enable = true;
    platformTheme = "kde";
    style = "adwaita";
  };

  # Set the default session to plasma (Wayland)
  # Otherwise: "plasmax11" for X11
  services.displayManager.defaultSession = "plasmax11";
}
