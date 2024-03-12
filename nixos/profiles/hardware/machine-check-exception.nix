# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let inherit (pkgs.stdenv) isLinux isDarwin isAarch64;
in {
  environment.systemPackages = with pkgs; [
    # Hardware monitoring
    lm_sensors
    # Hardware monitoring daemon
    rasdaemon
  ];


  # Mar 12 10:23:48 workstation kernel: mce: [Hardware Error]: Machine check events logged
  # Mar 12 10:23:48 workstation kernel: mce: [Hardware Error]: CPU 4: Machine Check: 0 Bank 5: bea0000001000108
  # Mar 12 10:23:48 workstation kernel: mce: [Hardware Error]: TSC 0 ADDR ffffffaf906a82 MISC d012000100000000 SYND 4d000000 IPID 500b000000000
  # Mar 12 10:23:48 workstation kernel: mce: [Hardware Error]: PROCESSOR 2:a20f10 TIME 1710208926 SOCKET 0 APIC 8 microcode a201016
  hardware.rasdaemon = {
    enable = true;
    record = true; # Record hardware events to SQLite3 database
    extraModules = [ "edac_mce_amd" ]; # Load the EDAC MCE driver for AMD CPUs
    config = ''
      # defaults from included config
      PAGE_CE_REFRESH_CYCLE="24h"
      PAGE_CE_THRESHOLD="50"
      PAGE_CE_ACTION="soft"
    '';
    mainboard = ''
      vendor = Micro-Star International Co., Ltd.
      product = MAG X570 TOMAHAWK WIFI (MS-7C84)
    '';
  };
}
