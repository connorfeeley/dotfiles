# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux isDarwin isAarch64;
  inherit (config.home) homeDirectory;
in
{
  systemd.user.services = {
    x11-server = {
      Unit = {
        Description = "x11-server";
        After = [ "display-manager.service" "network.target" "syslog.target"];
        PartOf = [ "multi-user.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.x11vnc}/bin/x11vnc -display :0 -auth /run/user/1000/gdm/Xauthority -forever -noxdamage -repeat -shared -passwdfile ${homeDirectory}/.vnc/passwd -rfbport 5900 -nap -many";
        ExecStop = "${pkgs.x11vnc}/bin/x11vnc -R stop";
        Restart= "on-failure";
      };
    };
  };
}
