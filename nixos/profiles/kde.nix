# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib, pkgs, ... }:
{
  ###
  ### KDE
  ###
  services.xserver.desktopManager.plasma6 = {
    enable = true;
    # FIXME: does not exist for plasma6, only plasma5
    # useQtScaling = true;
    # runUsingSystemd = false;
  };

  qt.platformTheme = "gnome";
  qt.style = "adwaita";
}
