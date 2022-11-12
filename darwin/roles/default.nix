{ collective
, profiles
,
}:
let
  workstation =
    (with (collective.profiles); [
      fonts.common
      # secrets
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
in
{
  inherit
    workstation
    ;
}
