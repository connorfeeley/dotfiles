{ config
, osConfig
, lib
, pkgs
, ...
}:
let
  inherit (config.lib.dag) entryAfter;
  inherit (osConfig.dotfield) guardian;
in
{
  home.activation.updateDock =
    let
      homeDir = "/Users/${guardian.username}/Library/Preferences/com.apple.dock.plist";
      replaceAppEntry = app: ''$DRY_RUN_CMD ${pkgs.dockutil}/bin/dockutil --add "file://$(readlink -f "/etc/profiles/per-user/cfeeley/Applications/${app}.app")" --replacing ${app} ${homeDir}'';
      maybeEmacsCommand = lib.optionalString config.programs.emacs.enable (replaceAppEntry "Emacs");
      maybeItermCommand = lib.optionalString config.programs.iterm2.enable (replaceAppEntry "iTerm2");
    in
    entryAfter [ "writeBoundary" ] ''
      ${maybeEmacsCommand}
      ${maybeItermCommand}
    '';
}
