collective: { inputs, ... }:
let
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) x86_64-linux;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  nixosModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles { inherit collective profiles; };

  importables = { inherit collective profiles roles primaryUser; };
in
{
  inherit importables;

  imports = [ (importHosts ./machines) ];

  hosts = {
    bootstrap-graphical.modules =
      (with roles; graphical ++ tangible ++ workstation)
      ++ (with profiles; [
        login.gdm
      ]);

    workstation.modules =
      (with roles; graphical ++ tangible ++ webdev ++ fpgadev ++ workstation)
      ++ (with profiles; [
        boot.systemd-boot
        hardware.amd
        nvidia

        gnome-desktop
        login.gdm
        # login.greetd

        builder

        virtualisation.vm-variant
        workstations.flatpak
      ]);

    h8tsner.modules =
      (with roles; server) ++ (with profiles; [
        environments.hetzner-cloud
        virtualisation.vm-variant
      ]);
  };

  hostDefaults = {
    system = x86_64-linux;
    channelName = "nixos-unstable";
    imports = [
      collective.modules
      nixosModules
    ];
    modules = [
      collective.profiles.core
      profiles.core
      home-manager.nixosModules.home-manager
      digga.nixosModules.nixConfig

      # FIXME: upstream module causes a huge number of unnecessary
      # dependencies to be pulled in for all systems -- many of them are
      # graphical. should only be imported as needed.
      # digga.nixosModules.bootstrapIso

      # FIXME: migrate to sops
      agenix.nixosModules.age
    ];
  };
}
