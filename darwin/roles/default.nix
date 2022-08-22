{
  collective,
  profiles,
}: let
  workstation =
    (with (collective.profiles); [
      fonts.common
      # FIXME(cfeeley): delete fully?
      # fonts.pragmatapro
      networking.common
      networking.ssh-host
      networking.tailscale
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
