collective: { inputs, ... }:
let
  inherit (inputs) agenix home-manager digga dwarffs nixos-vscode-server hercules-ci-agent nixos-wsl;
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
    bootstrap-graphical = {
      # Must run: export NIX_DISK_IMAGE=bootstrap-graphical.qcow2
      system = "aarch64-linux";
      modules =
        # (with roles; graphical) ++
        (with profiles; [
          virtualisation.vm-variant

          desktop.common
          xorg
          kde
          gnome-desktop
          # pantheon
          # hm-xmonad
        ]);
    };

    workstation.modules =
      #: ~ Modules ~
      [ dwarffs.nixosModules.dwarffs ] ++
      #: ~ Roles ~
      (with roles; graphical ++ tangible ++ virt ++ fpgadev ++ server) ++
      #: ~ Profiles ~
      (with profiles; [
        hardware.amd
        nvidia
        hidpi
        virtualisation.vm-variant

        builder
        # binary-cache

        mail

        workstations.flatpak
        games

        desktop.common
        xorg
        # hm-xmonad
        kde
        gnome-desktop
        xfce
        # pantheon

        grafana
      ]);

    workstation-wsl.modules =
      #: ~ Modules ~
      [ dwarffs.nixosModules.dwarffs ] ++
      #: ~ Roles ~
      (with roles; graphical ++ tangible ++ virt ++ server) ++
      #: ~ Profiles ~
      (with profiles; [
        nixos-wsl.nixosModules.wsl
        hardware.amd
        nvidia
        hidpi
        virtualisation.vm-variant

        builder
        # binary-cache

        # mail

        # workstations.flatpak
        # games

        # desktop.common
        # xorg
        # # hm-xmonad
        # kde
        # gnome-desktop
        # xfce
        # # pantheon

        # grafana
      ]);

    # workstation-iso.modules =
    #   (with roles; graphical ++ tangible ++ server)
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
        (with roles; server) ++
        (with profiles; [
          builder

          desktop.common
          xorg
          kde
          gnome-desktop
          xfce
          # pantheon
          # hm-xmonad
        ])
      ;
    };

    builder = {
      system = aarch64-linux;
      modules =
        (with roles; server) ++
        (with profiles; [
          builder
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
      hercules-ci-agent.nixosModules.agent-service

      nixos-vscode-server.nixosModules.default

      # FIXME: upstream module causes a huge number of unnecessary
      # dependencies to be pulled in for all systems -- many of them are
      # graphical. should only be imported as needed.
      digga.nixosModules.bootstrapIso
    ];
  };
}
