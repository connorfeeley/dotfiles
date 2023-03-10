# It's lonely in here.
{ pkgs, ... }: {
  home.packages = with pkgs; [
    deploy-flake # <- Copy flake closure over SSH and switch target config
    deploy-rs # <- Rusty Nix deployment tool
    colmena # <- Another rusty Nix deployment tool

    # Hetzner Cloud management tool
    # Configure the hcloud program to use your token:
    # - hcloud context create my-project
    hcloud # <- CLI for Hetzner Cloud.

    mcrcon # <- Minecraft console client
  ];
}
