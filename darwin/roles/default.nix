{
  collective,
  profiles,
}: let
  workstation =
    (with (collective.profiles); [
      fonts.common
      # FIXME(cfeeley): delete fully?
      # fonts.pragmatapro
      secrets
    ])
    ++ (with profiles; [
      distributed-build

      gui
      emacs
      development
      podman
      system-defaults
    ]);
in {
  inherit
    workstation
    ;
}
