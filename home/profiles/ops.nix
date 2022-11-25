# It's lonely in here.
{ config
, lib
, pkgs
, ...
}: {
  home.packages = with pkgs; [
    nixopsUnstable # <- Use Nix to operate on Nix OS operations.

    # Hetzner Cloud management tool
    # Configure the hcloud program to use your token:
    # - hcloud context create my-project
    hcloud # <- CLI for Hetzner Cloud.

    mcrcon # <- Minecraft console client
  ];
}
