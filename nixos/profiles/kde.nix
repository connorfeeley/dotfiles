# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib, pkgs, ... }:
{
  ###
  ### KDE
  ###
  services.xserver.desktopManager.plasma5 = {
    enable = true;
    useQtScaling = true;
    runUsingSystemd = false;
  };

  qt.platformTheme = "gnome";
}
