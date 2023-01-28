{ collective
, profiles
,
}:
let
  workstation = (with (collective.profiles); [
      fonts.common
      secrets
      flox
    ])
    ++ (with profiles; [
      distributed-build

      gui
      emacs
      development
      virtualization.podman
      system-defaults
    ]);

  server = (with (collective.profiles); [
    fonts.common
    secrets
    flox
  ])
  ++ (with profiles; [
    system-defaults
    distributed-build

    virtualization.podman
  ]);
in
{
  inherit
    workstation
    server
    ;
}
