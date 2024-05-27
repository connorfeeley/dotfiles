# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, lib, ... }: {
  home.packages = [ ] ++ lib.optionals pkgs.stdenv.isLinux [
    # FIXME(2024-05-26): Broken on MacOS
    pkgs.dbeaver-bin # <- GUI database manager
    pkgs.element-desktop # <- Matrix client
  ];
}
