# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib, pkgs, ... }:
{
  ###
  ### KDE
  ###
  services.desktopManager.plasma6 = {
    enable = false;
    # FIXME: does not exist for plasma6, only plasma5
    # useQtScaling = true;
    # runUsingSystemd = false;
  };

  services.xserver.desktopManager.plasma5 = {
    enable = true;
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
  services.displayManager.defaultSession = "plasma";
}
