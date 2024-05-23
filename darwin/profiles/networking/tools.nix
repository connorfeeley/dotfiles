# SPDX-FileCopyrightText: 2024 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.wakeonlan ];
}
