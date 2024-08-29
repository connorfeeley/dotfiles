# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, ... }:

let inherit (pkgs.stdenv) isLinux isDarwin isAarch64;
in {
  environment.systemPackages = with pkgs; [
    # ...

    # support both 32- and 64-bit applications
    wineWowPackages.stable

    (lutris.override {
      extraLibraries = pkgs: [
        # List library dependencies here
      ];
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })

    # # support 32-bit only
    # wine

    # # support 64-bit only
    # (wine.override { wineBuild = "wine64"; })

    # # wine-staging (version with experimental features)
    # wineWowPackages.staging

    # winetricks (all versions)
    winetricks

    # native wayland support (unstable)
    # wineWowPackages.waylandFull
  ];

  # Star Citizen config
  # https://github.com/starcitizen-lug/knowledge-base/wiki/Tips-and-Tricks#nixos-tweaks
  boot.kernel.sysctl = {
    "vm.max_map_count" = 16777216;
    "fs.file-max" = 524288;
  };

  # EAC
  networking.extraHosts = "127.0.0.1 modules-cdn.eac-prod.on.epicgames.com";
}
