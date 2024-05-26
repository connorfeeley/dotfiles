# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ ... }:
{
  security.pam.services.sddm.enableGnomeKeyring = true;

  services.xserver = {
    displayManager.lightdm = {
      enable = true;
    };
  };
}
