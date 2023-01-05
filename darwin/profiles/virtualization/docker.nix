{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.dotfield) guardian;
in
{
  homebrew = {
    brews = [{ name = "docker"; }];
    casks = [{ name = "docker-desktop"; }];
  };

  environment.variables = {
    # DOCKER_HOST = "unix://$XDG_DATA_HOME/containers/podman/machine/podman-machine-default/podman.sock";
  };

  users.groups.docker.members = [ "root" guardian.username ];
}
