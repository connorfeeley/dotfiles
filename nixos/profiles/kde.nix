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

  # Plasma 5 was removed in nixpkgs 26.05 (EOL upstream); plasma6 above is the
  # replacement. The old plasma5 block (enable=false, useQtScaling, runUsingSystemd)
  # had no remaining effect and is dropped.

  qt = {
    enable = true;
    platformTheme = "kde";
    style = "adwaita";
  };

  # Set the default session to plasma (Wayland)
  # Otherwise: "plasmax11" for X11
  services.displayManager.defaultSession = "plasmax11";
}
