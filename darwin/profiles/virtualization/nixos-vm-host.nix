{ ...
}:
let
  substituters = [ "https://macos-builder.cachix.org" ];
  trusted-substituters = substituters;
in
{
  nix.settings = {
    inherit substituters trusted-substituters;
    trusted-public-keys = [ "macos-builder.cachix.org-1:HPWcq59/iyqQz6HEtlO/kjD/a7ril0+/XJc+SZ2LgpI=" ];
    builders = "ssh://builder@localhost aarch64-linux /etc/nix/builder_ed25519 4 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo='";
    builders-use-substitutes = true;
  };
}
