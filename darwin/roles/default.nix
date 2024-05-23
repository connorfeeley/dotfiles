{ collective }:
let
  workstation = (with collective.profiles.global; [ fonts.common secrets flox ])
    ++ (with collective.profiles.darwin; [
    system-defaults
    distributed-build

    gui
    homebrew
    emacs
    development
    # virtualization.docker
  ]);

  server = (with collective.profiles.global; [ fonts.common secrets flox ])
    ++ (with collective.profiles.darwin; [
    system-defaults
    distributed-build

    # virtualization.podman
  ]);
in
{ inherit workstation server; }
