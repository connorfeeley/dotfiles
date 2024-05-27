# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, lib, ... }: {
  home.packages = [
    pkgs.dbeaver-bin # <- GUI database manager
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    # Broken on MacOS
    pkgs.element-desktop # <- Matrix client
  ];
}
