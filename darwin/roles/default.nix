{ collective }:
let
  workstation = (with collective.profiles; [ fonts.common secrets flox ])
    ++ (with collective.darwinProfiles; [
    distributed-build

    gui
    emacs
    development
    # virtualization.docker
    system-defaults
  ]);

  server = (with collective.profiles; [ fonts.common secrets flox ])
    ++ (with collective.darwinProfiles; [
    system-defaults
    distributed-build

    # virtualization.podman
  ]);
in
{ inherit workstation server; }
