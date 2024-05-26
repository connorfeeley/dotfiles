# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }:
{
  services.xserver = {
    displayManager = {
      sddm.enable = true;
      sddm.enableHidpi = true;
      sddm.autoNumlock = true;
      # sddm.settings = { Autologin = { Session = "plasma.desktop"; User = "john"; } ; };
      theme = "Aritim-Dark";
    };
  };
}
