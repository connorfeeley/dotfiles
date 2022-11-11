collective: { inputs, ... }:
let
  inherit (inputs) agenix home-manager digga nix-serve-ng modded-minecraft-servers;
  inherit (inputs.flake-utils.lib.system) x86_64-linux aarch64-linux;
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
      (with roles; graphical ++ tangible ++ webdev ++ fpgadev ++ workstation ++ server)
      ++ (with profiles; [
        boot.systemd-boot
        hardware.amd
        nvidia
        virtualisation.vm-variant

        # Just use startx for now...
        # gnome-desktop
        # login.gdm
        # login.greetd

        builder

        workstations.flatpak

        minecraft.minecraft-client
      ]);

    h8tsner = {
      modules =
        (with collective.profiles; [
          networking.ssh-host
          secrets
        ]) ++
        (with roles; server) ++ (with profiles; [
          environments.hetzner-cloud

          minecraft.minecraft-server
        ])
      ;
    };
    rosy = {
      system = aarch64-linux;
      modules =
        (with collective.profiles; [
          networking.ssh-host
        ]) ++
        (with roles; server) ++ (with profiles; [
          builder
        ])
      ;
    };
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
      collective.profiles.flox
      profiles.core
      home-manager.nixosModules.home-manager
      digga.nixosModules.nixConfig

      nix-serve-ng.nixosModules.default

      # FIXME: upstream module causes a huge number of unnecessary
      # dependencies to be pulled in for all systems -- many of them are
      # graphical. should only be imported as needed.
      digga.nixosModules.bootstrapIso

      # FIXME: migrate to sops
      agenix.nixosModules.age
    ];
  };
}
