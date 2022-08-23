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
      emacs
      gui
      system-defaults
    ]);
in {
  inherit
    workstation
    ;
}
