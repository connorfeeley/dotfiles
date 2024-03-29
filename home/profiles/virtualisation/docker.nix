# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

moduleArgs@{ config, lib, pkgs, ... }:

let inherit (config.lib.dag) entryAfter; in {
  # Setup docker CLI plugins:
  home.activation.setupDockerCLI = entryAfter [ "writeBoundary" ] ''
    DOCKER_CONFIG=''${DOCKER_CONFIG:-$HOME/.docker}
    mkdir -p $DOCKER_CONFIG/cli-plugins
    ln -sf $(which docker-compose) $DOCKER_CONFIG/cli-plugins/docker-compose
    ln -sf $(which docker-buildx) $DOCKER_CONFIG/cli-plugins/docker-buildx
  '';
  home.packages = with pkgs;
    [ docker-buildx docker-credential-helpers buildkit ] ++ lib.optionals
      (moduleArgs.osConfig.virtualisation.docker.enable or false) [
      # docker
      docker-compose
    ];

  # Aliases
  programs.zsh.shellAliases = {
    dcr = "docker compose run --rm";
  };
}
