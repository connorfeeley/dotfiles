# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

{ config, lib, pkgs, inputs, ... }:
let inherit (inputs) arion;
in {
  imports = [ arion.nixosModules.arion ];

  # Arion works with Docker, but for NixOS-based containers, you need Podman
  # since NixOS 21.05.

  environment.systemPackages = with pkgs; [ arion ]
    ++ (lib.optionals (!config.virtualisation.docker.enable) [
    # Install the docker CLI to talk to podman (Not needed when virtualisation.docker.enable = true)
    docker-client
  ]);

  virtualisation.arion = {
    backend = if config.virtualisation.docker.enable then "docker" else "podman-socket";

    projects.vpn-torrent.settings = {
      # Specify you project here, or import it from a file.
      # NOTE: This does NOT use ./arion-pkgs.nix, but defaults to NixOS' pkgs.
      # imports = [ ];
      service.image = "haugene/transmission-openvpn";
      service.volumes = [ "/mnt/zfs/media/libraries:/var/run/libraries" ];
      # service.environment.POSTGRES_PASSWORD = "mydefaultpass";
    };
  };
}
