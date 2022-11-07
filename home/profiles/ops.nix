# It's lonely in here.
{ config
, lib
, pkgs
, ...
}: {
  home.packages = with pkgs; [
    nixops # <- Use Nix to operate on Nix OS operations.
    hcloud # <- CLI for Hetzner Cloud.
  ];
}
