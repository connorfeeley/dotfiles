{ config
, lib
, modulesPath
, pkgs
, ...
}:
{
  imports = [
    (modulesPath + "/profiles/macos-builder.nix")
  ];
}
