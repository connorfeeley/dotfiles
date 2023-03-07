# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, ... }: {
  home.packages = with pkgs; [
    element-desktop # Matrix client
  ];
}
