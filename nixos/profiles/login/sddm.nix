# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }:
{
  services.xserver = {
    displayManager.sddm = {
      enable = true;
      enableHidpi = true;
      autoNumlock = true;
      # settings = { Autologin = { Session = "plasma.desktop"; User = "john"; } ; };
    };
  };
}
