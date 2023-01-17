{ config
, osConfig
, lib
, pkgs
, ...
}:
let
  inherit (config.lib.dag) entryAfter;
in
{
  home.activation.updateDock =
    let
      replaceAppEntry = app: ''$DRY_RUN_CMD ${pkgs.dockutil}/bin/dockutil --add "file:///etc/profiles/per-user/cfeeley/Applications/${app}.app" --replacing ${app}'';
      maybeEmacsCommand = lib.optionalString config.programs.emacs.enable (replaceAppEntry "Emacs");
      maybeItermCommand = lib.optionalString config.programs.iterm2.enable (replaceAppEntry "iTerm2");
    in
    entryAfter [ "writeBoundary" ] ''
      ${maybeEmacsCommand}
      ${maybeItermCommand}
    '';
}
