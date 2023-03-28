# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux isDarwin isAarch64;
  inherit (config.home-manager.users.cfeeley.home) homeDirectory;
in
{
  systemd.user.services = {
    x11-server = {
      Unit = {
        Description = "x11-server";
        PartOf = [ "multi-user.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.x11vnc}/bin/x11vnc -display :0 -auth guess -forever -noxdamage -repeat -shared -rbfauth ${homeDirectory}/.vnc/passwd -rfbport 5900";
        Restart = "always";
      };
    };
  };
}
