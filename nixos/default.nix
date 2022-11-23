collective: { inputs, ... }:
let
  inherit (inputs) agenix home-manager digga nix-serve-ng dwarffs stylix;
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

  hosts = rec {
    bootstrap-graphical.modules =
      (with roles; graphical ++ tangible ++ desktop)
      ++ (with profiles; [
        login.gdm
      ]);

    workstation.modules =
      #: ~ Modules ~
      [
        dwarffs.nixosModules.dwarffs
        # stylix.nixosModules.stylix
      ] ++
      #: ~ Roles ~
      (with roles; graphical ++ tangible ++ virt ++ fpgadev ++ desktop ++ server) ++
      #: ~ Profiles ~
      (with profiles; [
        boot.systemd-boot
        hardware.amd
        nvidia
        hidpi
        virtualisation.vm-variant
        builder

        workstations.flatpak
        games

        login.gdm # Not actually gdm
        # gnome-desktop

        grafana
      ]);

    # workstation-iso.modules =
    #   (with roles; graphical ++ tangible ++ desktop ++ server)
    #   ++ (with profiles; [
    #     boot.systemd-boot
    #     hardware.amd
    #     # NOTE: vaapiVdpau can't be built on aarch64-linux
    #     # nvidia
    #     virtualisation.vm-variant
    #   ]);

    h8tsner = {
      modules =
        (with collective.profiles; [
          networking.ssh-host
          # secrets
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
        (with roles; desktop ++ server) ++
        (with profiles; [
          boot.systemd-boot
          builder

          # rosetta
        ])
      ;
    };
  };

  hostDefaults = {
    system = x86_64-linux;
    channelName = "nixos-stable";
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
      agenix.nixosModules.age

      nix-serve-ng.nixosModules.default

      # FIXME: upstream module causes a huge number of unnecessary
      # dependencies to be pulled in for all systems -- many of them are
      # graphical. should only be imported as needed.
      digga.nixosModules.bootstrapIso
    ];
  };
}
