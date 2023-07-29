# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ lib, pkgs, ... }:

{
  home.packages = with pkgs;
    (lib.optionals pkgs.stdenv.isLinux [ distrobox ]) ++
    [ arion ];
}
