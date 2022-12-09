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
      podman
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

    podman
  ]);
in
{
  inherit
    workstation
    server
    ;
}
