{ collective }:
let
  workstation = (with collective.profiles; [ fonts.common secrets flox ])
    ++ (with collective.darwinProfiles; [
    system-defaults
    distributed-build

    gui
    homebrew
    emacs
    development
    # virtualization.docker
  ]);

  server = (with collective.profiles; [ fonts.common secrets flox ])
    ++ (with collective.darwinProfiles; [
    system-defaults
    distributed-build

    # virtualization.podman
  ]);
in
{ inherit workstation server; }
