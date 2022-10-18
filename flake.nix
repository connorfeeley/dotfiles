{
  description = "Dotfield";

  # Enable as needed for bootstrapping. Otherwise these entries cause warnings on every rebuild.
  # nixConfig.extra-experimental-features = "nix-command flakes";
  # nixConfig.extra-substituters = "https://dotfield.cachix.org https://nix-community.cachix.org";
  # nixConfig.extra-trusted-public-keys = "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    nixpkgs.follows = "nixos-unstable";
    nixpkgs-stable.follows = "nixos-stable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-stable-21-11.url = "github:NixOS/nixpkgs/nixos-21.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";
    nixlib.url = "github:nix-community/nixpkgs.lib";

    ##: --- utilities ----------------------------------------------------------

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    deploy = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix.url = "github:montchr/agenix/darwin-support";
    flake-utils.url = "github:numtide/flake-utils";
    prefmanager.url = "github:malob/prefmanager";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nvfetcher.url = "github:berberman/nvfetcher";
    sops-nix = {
      url = "github:Mic92/sops-nix";
    };

    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.darwin.follows = "darwin";
    };

    nix-dram = {
      # FIXME: remove when nix-dram supports nix 2.11
      # nix-PR: dramforever/nix-dram#22
      url = "github:pub-solar/nix-dram/refs/heads/rebase-nix-2.11.0";
      # url = "github:dramforever/nix-dram";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: --- sources ------------------------------------------------------------

    mach-nix.url = "github:DavHau/mach-nix/refs/tags/3.5.0";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
    firefox-darwin.inputs.nixpkgs.follows = "nixpkgs";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";

    deadnix = {
      url = "github:astro/deadnix/refs/tags/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.inputs.nixpkgs.follows = "nixpkgs";
    };

    comma = {
      url = "github:nix-community/comma";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-xilinx = {
      url = "gitlab:doronbehar/nix-xilinx";
    };

    nix-json-progress = {
      # TODO: upstream to github:dramforever/nix-json-progress
      url = "github:connorfeeley/nix-json-progress/feature/darwin-architecture-support";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nickel = {
      url = "github:tweag/nickel";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad-config = {
      url = "git+https://git.sr.ht/~cfeeley/xmonad-config";
    };
    xmobar-config = {
      url = "path:config/xmobar";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      # Packages are built against this channel.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ttc-subway-font = {
      url = "git+ssh://git@git.sr.ht/~cfeeley/ttc-subway-font";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-work.url = "git+ssh://git@git.sr.ht/~cfeeley/nixpkgs-work";

    nix-nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nicely preconfigured vim
    pta2002-neovim = {
      url = "github:pta2002/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: --- other --------------------------------------------------------------

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    ##: --- FHS compat ---------------------------------------------------------
    nix-ld = {
      url = "github:Mic92/nix-ld/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    envfs = {
      # url = "github:Mic92/envfs";
      url = "github:wentasah/envfs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    agenix,
    darwin,
    deploy,
    digga,
    mach-nix,
    emacs-overlay,
    firefox-darwin,
    flake-utils,
    gitignore,
    home-manager,
    nixlib,
    nix-colors,
    nix-dram,
    nixos-generators,
    nixos-hardware,
    nixos-stable,
    nixos-stable-21-11,
    nixos-unstable,
    nixpkgs-wayland,
    nur,
    nvfetcher,
    sops-nix,
    xmonad-config,
    xmobar-config,
    pta2002-neovim,
    ttc-subway-font,
    nixpkgs-work,
    nickel,
    comma,
    nix-xilinx,
    nix-json-progress,
    deadnix,
    nix-nil,
    nix-ld,
    nix-alien,
    envfs,
    ...
  } @ inputs: let
    inherit
      (digga.lib)
      flattenTree
      importExportableModules
      rakeLeaves
      ;
    inherit
      (flake-utils.lib)
      eachSystem
      ;
    inherit
      (flake-utils.lib.system)
      x86_64-linux
      aarch64-linux

      x86_64-darwin
      aarch64-darwin
      ;

    supportedSystems = [
      x86_64-linux
      aarch64-linux

      x86_64-darwin

      # FIXME: Something in this flake's chain of dependencies triggers a build
      # failure when `aarch64-darwin` is added to `supportedSystems`,
      # specifically due to `pyopenssl`. Many python packages will not build on
      # this system due to the broken `pyopenssl` dependency.
      #
      # As of 2022-08-20, it appears most of these issues have been fixed, but
      # some packages have still caused errors:
      #
      # - promnesia
      # - yubikey-manager and/or yubikey-personalization
      #
      # [Updated: 2022-08-20]
      # https://github.com/NixOS/nixpkgs/issues/175875
      # https://github.com/pyca/pyopenssl/issues/873
      aarch64-darwin
    ];

    darwinSystems = [x86_64-darwin aarch64-darwin];

    collective = {
      modules = importExportableModules ./modules;
      peers = import ./ops/metadata/peers.nix;
      profiles = rakeLeaves ./profiles;
    };

    # FIXME: split this to shared/nixos/darwin-specific
    overlays = [
      agenix.overlay
      emacs-overlay.overlay
      gitignore.overlay
      nix-dram.overlay
      nixpkgs-wayland.overlay
      nur.overlay
      nvfetcher.overlay
      firefox-darwin.overlay

      xmobar-config.overlay

      ttc-subway-font.overlay

      nix-xilinx.overlay

      nixpkgs-work.overlays.default

      (final: prev: {
        emacs-plus = self.packages.${final.system}.emacs-plus;

        amphetamine-enhancer = self.packages.${final.system}.amphetamine-enhancer;

        xmonad-config = xmonad-config.packages.${prev.system}.default;
        xmonad = xmonad-config.packages.${prev.system}.default;

        nix-json-progress = nix-json-progress.packages.${prev.system}.nix-json-progress;

        dashboard = nixpkgs-work.packages.${prev.system}.dashboard;
        zeuspack = nixpkgs-work.packages.${prev.system}.zeuspack;
      })
    ];
  in
    (digga.lib.mkFlake {
      inherit
        self
        inputs
        supportedSystems
        ;

      channelsConfig = {
        allowUnfree = true;
        allowUnsupportedSystem = true;

        allowBroken = false;
      };

      channels = {
        nixos-stable = {
          inherit overlays;
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/stable)
            (digga.lib.importOverlays ./packages)
          ];
        };
        nixpkgs-darwin-stable = {
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/stable)
            (digga.lib.importOverlays ./packages)
          ];
          # FIXME: some of these have no use on darwin (e.g. nixpkgs-wayland)
          overlays =
            overlays
            ++ [
              (final: prev: {
                amphetamine-enhancer = self.packages.${final.system}.amphetamine-enhancer;
              })
            ];
        };
        nixos-unstable = {
          inherit overlays;
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/nixos-unstable)
            (digga.lib.importOverlays ./packages)
          ];
        };
        nixpkgs-trunk = {};
      };

      lib = import ./lib {
        inherit (collective) peers;
        lib = digga.lib // nixos-unstable.lib;
      };

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })
      ];

      nixos = import ./nixos collective;
      darwin = import ./darwin collective;
      home = import ./home collective;

      devshell = ./shell;

      homeConfigurations = digga.lib.mkHomeConfigurations self.nixosConfigurations;

      deploy.nodes = digga.lib.mkDeployNodes { } {
        # workstation = with (collective.peers.hosts.workstation); {
        #   hostname = "workstation";
        #   sshUser = "cfeeley";
        #   fastConnection = true;
        #   autoRollback = true;
        #   magicRollback = true;
        # };
        # FIXME(darwin) throws:
        # error: 'runCommandNoCC' has been renamed to/replaced by 'runCommand'
        # debian-vm = with (collective.peers.hosts.debian-vm); {
        #   hostname = "debian-vm";
        #   sshUser = "cfeeley";
        #   fastConnection = true;
        #   autoRollback = true;
        #   magicRollback = true;
        #   profilesOrder = [ "cfeeley" ];
        #   profiles.cfeeley = {
        #     user = "cfeeley";
        #     path = deploy.lib.aarch64-linux.activate.home-manager
        #       self.homeConfigurationsPortable.aarch64-linux."cfeeley@debian-vm";
        #   };
        # };

        # Deploy to 'cfeeley-laptop':
        # - Recommended: deploy .#cfeeley-laptop -- --print-build-logs
        # If there are nix eval errors, then we can tell 'deploy' to skip the flake checks:
        # - Not recommended: deploy --skip-checks .#cfeeley-laptop -- --print-build-logs
        cfeeley-laptop = with (collective.peers.hosts.cfeeley-laptop); {
          hostname = ipv4.address;
          sshUser = "cfeeley";
          fastConnection = true;
          autoRollback = true;
          magicRollback = true;
          profilesOrder = [ "cfeeley" ];
          profiles.cfeeley = {
            user = "cfeeley";
            path = deploy.lib.x86_64-linux.activate.home-manager
              self.homeConfigurationsPortable.x86_64-linux."cfeeley@cfeeley-laptop";
          };
        };
      };
    })
    // (eachSystem darwinSystems (system: {
      packages =
        let
          # nixpkgs set used for flake's 'packages' output
          flakepkgs = import nixpkgs {
            inherit system;
            overlays = [
              self.overlays."nixos-unstable/emacs28Macport"
            ];
          };
          # emacs-mac v28.2 with native compilation enabled;
          emacs28Macport = flakepkgs.emacs28Macport;
          # emacs-mac v28.2 with native compilation disabled;
          # - Intended primarily as a quick way to verify that the package builds
          # - Should most likely not be used as part of a system configuration (use emacs28Macport instead)
          emacs28Macport-noNativeComp = flakepkgs.emacs28Macport.override {nativeComp = false;};
        in
        (builtins.mapAttrs (n: v: nixpkgs.legacyPackages.${system}.callPackage v { })
          (flattenTree (rakeLeaves ./darwin/packages)))
        // {
          inherit
            # NOTE: both emacs28Macport variants are impure
            # Tested with XCode CLT version: 14.0.0.0.1.1661618636
            emacs28Macport
            emacs28Macport-noNativeComp
          ;
        }
      ;
    }));
}
