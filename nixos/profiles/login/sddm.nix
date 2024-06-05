# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }:
{
  security.pam.services = {
    login.gnupg = {
      enable = true;
      noAutostart = true;
    };

    sddm.gnupg = {
      enable = true;
      noAutostart = true;
    };
  };

  services.xserver = {
    displayManager.sddm = {
      enable = true;
      enableHidpi = true;
      autoNumlock = true;
      wayland.enable = true;
      # settings = { Autologin = { Session = "plasma.desktop"; User = "john"; } ; };
    };
  };
}
