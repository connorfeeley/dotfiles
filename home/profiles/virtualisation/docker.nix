{ pkgs
, ...
}: {
  /*
    Setup docker CLI plugins:
    DOCKER_CONFIG=${DOCKER_CONFIG:-$HOME/.docker}
    mkdir -p $DOCKER_CONFIG/cli-plugins
    ln -s $(which docker-compose) $DOCKER_CONFIG/cli-plugins/docker-compose
    ln -s $(which docker-buildx) $DOCKER_CONFIG/cli-plugins/docker-buildx
  */
  home.packages = with pkgs; [
    docker
    docker-compose
    docker-buildx
    docker-credential-helpers
    buildkit
  ];
}
