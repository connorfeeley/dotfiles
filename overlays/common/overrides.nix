channels: final: prev:
let
  packagesFrom = inputAttr: inputAttr.packages.${final.system};
in
{
  __dontExport = true;

  ripgrep = prev.ripgrep.override { withPCRE2 = true; };

  xmonad = prev.xmonad-config;
  xmobar = prev.xmobar-config;

  inherit (channels.nixos-21-11)
    sourcetrail
    ;
}
