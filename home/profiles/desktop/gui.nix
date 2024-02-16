# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, lib, ... }: {
  home.packages = with pkgs; [
    dbeaver # <- GUI database manager
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    # Broken on MacOS
    element-desktop # <- Matrix client
  ];
}
