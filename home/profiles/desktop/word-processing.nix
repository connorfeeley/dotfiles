{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin;

  # Libreoffice is broken on darwin; while libreoffice-bin is darwin-only.
  # Select the correct package depending on the host system.
  libreofficePackage = with pkgs;
    if isLinux then libreoffice
    else if isDarwin then libreoffice-bin #
    else throw "Unsupported hostPlatform";
in
{
  home.packages = [ libreofficePackage ];
}
