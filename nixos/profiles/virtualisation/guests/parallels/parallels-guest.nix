# SPDX-FileCopyrightText: Copyright (c) 2003-2022 Eelco Dolstra and the Nixpkgs/NixOS contributors
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/NixOS/nixpkgs/pull/153665/files
# https://github.com/NixOS/nixpkgs/blob/d675eefdb0b4a0ad35caa5bd773b0dc01a3e72d9/nixos/modules/virtualisation/parallels-guest.nix
# https://github.com/Builditluc/dotnix/tree/d5cd6906308b55c4e5548779ff4ba86e138f708e/hosts/prlnix
{ config, lib, pkgs, ... }:
with lib;
let
  prl-tools = config.hardware.parallels.package;
  aarch64 = pkgs.stdenv.hostPlatform.system == "aarch64-linux";
in
{
  options = {
    hardware.parallels = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          This enables Parallels Tools for Linux guests, along with provided
          video, mouse and other hardware drivers.
        '';
      };

      autoMountShares = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Control prlfsmountd service. When this service is running, shares can not be manually
          mounted through `mount -t prl_fs ...` as this service will remount and trample any set options.
          Recommended to enable for simple file sharing, but extended share use such as for code should
          disable this to manually mount shares.
        '';
      };

      package = mkOption {
        type = types.nullOr types.package;
        default = config.boot.kernelPackages.prl-tools;
        defaultText = "config.boot.kernelPackages.prl-tools";
        example = literalExpression "config.boot.kernelPackages.prl-tools";
        description = ''
          Defines which package to use for prl-tools. Override to change the version.
        '';
      };
    };
  };

  config = mkIf config.hardware.parallels.enable {
    services.xserver = {
      videoDrivers = [ "prlvideo" ];

      modules = [ prl-tools ];

      config = ''
        Section "InputClass"
          Identifier      "prlmouse"
          MatchIsPointer  "on"
          MatchTag        "prlmouse"
          Driver          "prlmouse"
        EndSection
      '';

      screenSection = ''
        Option "NoMTRR"
      '';
    };

    hardware.opengl.package = prl-tools;
    hardware.opengl.package32 =
      pkgs.pkgsi686Linux.linuxPackages.prl-tools.override {
        libsOnly = true;
        kernel = null;
      };
    hardware.opengl.extraPackages = [ pkgs.mesa.drivers ];
    hardware.opengl.extraPackages32 = [ pkg.pkgsi686Linux.mesa.drivers ];

    services.udev.packages = [ prl-tools ];

    environment.systemPackages = [ prl-tools ];

    boot.extraModulePackages = [ prl-tools ];

    boot.kernelModules =
      if aarch64 then [
        "prl_fs"
        "prl_fs_freeze"
        "prl_notifier"
        "prl_tg"
      ] else [
        "prl_fs"
        "prl_fs_freeze"
        "prl_tg"
      ];

    services.timesyncd.enable = false;

    systemd.services.prltoolsd = {
      description = "Parallels Tools' service";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${prl-tools}/bin/prltoolsd -f";
        PIDFile = "/var/run/prltoolsd.pid";
      };
    };

    systemd.services.prlfsmountd =
      mkIf config.hardware.parallels.autoMountShares {
        description = "Parallels Shared Folders Daemon";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = rec {
          ExecStart = "${prl-tools}/sbin/prlfsmountd ${PIDFile}";
          ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /media";
          ExecStopPost = "${prl-tools}/sbin/prlfsmountd -u";
          PIDFile = "/run/prlfsmountd.pid";
        };
      };

    # FIXME: service broken
    # systemd.services.prlshprint = {
    #   description = "Parallels Shared Printer Tool";
    #   wantedBy = [ "multi-user.target" ];
    #   bindsTo = [ "cups.service" ];
    #   serviceConfig = {
    #     Type = "forking";
    #     ExecStart = "${prl-tools}/bin/prlshprint";
    #   };
    # };

    systemd.user.services = {
      prlcc = {
        description = "Parallels Control Center";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prlcc"; };
      };
      prldnd = {
        description = "Parallels Control Center";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prldnd"; };
      };
      prlcp = {
        description = "Parallels CopyPaste Tool";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prlcp"; };
      };
      prlsga = {
        description = "Parallels Shared Guest Applications Tool";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prlsga"; };
      };
      prlshprof = {
        description = "Parallels Shared Profile Tool";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = { ExecStart = "${prl-tools}/bin/prlshprof"; };
      };
    };
  };
}
