{ config
, lib
, pkgs
, profiles
, ...
}:
let
  cfg = config.dotfield;
in
{
  imports = [
    ./guardian.nix
    ./documentation.nix
    # ./host.nix
  ];

  config = { };
}
