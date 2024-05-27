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

    lightdm.gnupg = {
      enable = true;
      noAutostart = true;
    };
  };

  services.xserver.displayManager.lightdm = {
    enable = true;
  };
}
