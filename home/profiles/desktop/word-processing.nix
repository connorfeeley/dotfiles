{ pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isDarwin;

  # Libreoffice is broken on darwin; while libreoffice-bin is darwin-only.
  # Select the correct package depending on the host system.
  libreofficePackage = with pkgs;
    if isDarwin then libreoffice-bin
    else libreoffice;
in
{
  home.packages = [ libreofficePackage ];
}
