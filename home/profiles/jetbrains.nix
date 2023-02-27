# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: MIT

{ config, lib, pkgs, ... }: {
  home.packages = lib.mkIf (with pkgs.stdenv; (isLinux && isx86_64) || (isDarwin))
    (with pkgs.jetbrains; [
      clion
      datagrip
      gateway
      goland
      idea-ultimate
      pycharm-professional
      webstorm
    ]);
}
