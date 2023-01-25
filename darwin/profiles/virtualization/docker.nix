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

  environment.etc = {
    "docker/daemon.json".source = pkgs.writeText "daemon.json" (builtins.toJSON {
      hosts = [ "unix:///var/run/docker.sock" "tcp://127.0.0.1:2375" ];
      builder = {
        gc = {
          defaultKeepStorage = "20GB";
          enabled = true;
        };
      };
      experimental = false;
      features = {
        buildkit = true;
      };
    });
  };
}
