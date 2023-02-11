{
  description = "Dotfield";

  inputs = {
    ##: --- nixpkgs flavours ----------------------------------------------------------
    nixpkgs.follows = "nixos-stable";

    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-21-11.url = "github:NixOS/nixpkgs/nixos-21.11"; # Last release w/ sourcetrail

    ##: --- system -------------------------------------------------------------
    home-manager = { url = "github:nix-community/home-manager/release-22.11"; inputs.nixpkgs.follows = "nixpkgs"; };
    darwin = { url = "github:LnL7/nix-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    flake-parts = { url = "github:hercules-ci/flake-parts"; };
    digga = {
      url = "github:divnix/digga";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.darwin.follows = "darwin";
    };
    nixos-wsl = { url = "github:nix-community/NixOS-WSL"; inputs.nixpkgs.follows = "nixpkgs"; };
    agenix.url = "github:montchr/agenix/darwin-support";
    sops-nix = { url = "github:pogobanane/sops-nix/feat/home-manager-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # : ~~~ FHS compat ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    nix-alien = { url = "github:thiagokokada/nix-alien"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-autobahn = { url = "github:Lassulus/nix-autobahn"; inputs.nixpkgs.follows = "nixpkgs"; };
    envfs = { url = "github:Mic92/envfs"; inputs.nixpkgs.follows = "nixpkgs"; };

    ##: --- utilities ----------------------------------------------------------
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

    ##: --- other packages -----------------------------------------------------
    nur.url = "github:nix-community/NUR";
    nixos-generators = { url = "github:nix-community/nixos-generators"; inputs.nixpkgs.follows = "nixpkgs"; };
    nvfetcher.url = "github:berberman/nvfetcher";
    arion = { url = "github:hercules-ci/arion"; inputs.nixpkgs.follows = "nixpkgs"; }; # FIXME: checks fail on darwin
    nix-serve-ng = { url = "github:aristanetworks/nix-serve-ng"; inputs.nixpkgs.follows = "nixpkgs"; inputs.utils.follows = "flake-utils"; };
    nixago = { url = "github:nix-community/nixago"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixos-vscode-server = { url = "github:msteen/nixos-vscode-server"; inputs.nixpkgs.follows = "nixpkgs"; };

    ##: --- sources ------------------------------------------------------------
    mach-nix.url = "github:DavHau/mach-nix/refs/tags/3.5.0";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    deadnix = { url = "github:astro/deadnix/refs/tags/v1.0.0"; inputs.nixpkgs.follows = "nixpkgs"; };
    comma = { url = "github:nix-community/comma"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-index-database = { url = "github:Mic92/nix-index-database"; inputs.nixpkgs.follows = "nixpkgs"; };
    rnix-lsp = { url = "github:nix-community/rnix-lsp"; inputs.nixpkgs.follows = "nixpkgs"; };
    hercules-ci-agent = { url = "github:hercules-ci/hercules-ci-agent"; inputs = { nixpkgs.follows = "nixpkgs"; flake-parts.follows = "flake-parts"; nix-darwin.follows = "darwin"; }; };

    ##: --- personal packages --------------------------------------------------
    nurpkgs = { url = "github:connorfeeley/nurpkgs"; inputs.nixpkgs.follows = "nixpkgs"; };
    xmonad-config = { url = "git+https://git.sr.ht/~cfeeley/xmonad-config"; inputs.flake-utils.follows = "flake-utils"; };
    chatgpt-wrapper = { url = "git+https://git.sr.ht/~cfeeley/chatgpt-wrapper"; inputs.flake-utils.follows = "flake-utils"; inputs.nixpkgs.follows = "nixpkgs"; inputs.nixpkgs-darwin.follows = "nixpkgs-darwin"; };
    ttc-subway-font = { url = "git+ssh://git@git.sr.ht/~cfeeley/ttc-subway-font"; inputs.nixpkgs.follows = "nixpkgs"; }; # Private repo
    nixpkgs-input-leap = { url = "sourcehut:~cfeeley/nixpkgs/feat/input-leap"; };

    ##: --- meta packages ------------------------------------------------------
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; inputs.nixpkgs.follows = "nixpkgs"; };
    darwin-emacs = { url = "github:c4710n/nix-darwin-emacs"; };
    nix-xilinx = { url = "gitlab:doronbehar/nix-xilinx"; };

    ##: --- packages -----------------------------------------------------------
    nickel = { url = "github:tweag/nickel"; inputs.nixpkgs.follows = "nixpkgs"; };
    nix-nil = { url = "github:oxalica/nil"; };
    nix-init = { url = "github:nix-community/nix-init"; };
    devenv = { url = "github:cachix/devenv/v0.5"; };
    deploy = { url = "github:serokell/deploy-rs"; inputs.nixpkgs.follows = "nixpkgs"; };
    deploy-flake = { url = "github:antifuchs/deploy-flake"; inputs.nixpkgs.follows = "nixpkgs"; };
    prefmanager.url = "github:malob/prefmanager";
    tum-dse-config = { url = "github:TUM-DSE/doctor-cluster-config"; inputs.nixpkgs.follows = "nixpkgs"; inputs.nixpkgs-unstable.follows = "nixpkgs"; inputs.flake-parts.follows = "flake-parts"; };
    neovim-plusultra = { url = "github:jakehamilton/neovim"; };

    ##: --- other --------------------------------------------------------------
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    dwarffs.url = "github:edolstra/dwarffs";
    base16-kitty = { url = "github:kdrag0n/base16-kitty"; flake = false; };
    firefox-lepton = { url = "github:black7375/Firefox-UI-Fix"; flake = false; };
    modded-minecraft-servers = { url = "github:mkaito/nixos-modded-minecraft-servers"; inputs.nixpkgs.follows = "nixpkgs"; };
    plasma-manager = { url = "github:pjones/plasma-manager"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs =
    { self
    , nixpkgs
    , nixos-stable
    , nixpkgs-darwin
    , nixos-unstable
    , nixos-21-11
    , home-manager
    , darwin
    , digga
    , agenix
    , nixos-hardware
    , nix-alien
    , nix-autobahn
    , envfs
    , flake-utils
    , flake-parts
    , nur
    , nixos-generators
    , nvfetcher
    , arion
    , nix-serve-ng
    , nixago
    , nixos-vscode-server
    , mach-nix
    , gitignore
    , nix-colors
    , deadnix
    , comma
    , rnix-lsp
    , nurpkgs
    , xmonad-config
    , chatgpt-wrapper
    , ttc-subway-font
    , nixpkgs-input-leap
    , emacs-overlay
    , darwin-emacs
    , nix-xilinx
    , nickel
    , nix-nil
    , nix-init
    , devenv
    , deploy
    , deploy-flake
    , prefmanager
    , tum-dse-config
    , neovim-plusultra
    , flake-compat
    , dwarffs
    , base16-kitty
    , firefox-lepton
    , modded-minecraft-servers
    , plasma-manager
    , ...
    } @ inputs:
    let
      inherit (flake-utils.lib)
        eachSystem
        ;
      inherit (flake-utils.lib.system)
        x86_64-linux
        aarch64-linux

        x86_64-darwin
        aarch64-darwin
        ;

      supportedSystems = [
        x86_64-linux
        aarch64-linux

        x86_64-darwin
        aarch64-darwin
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # https://flake.parts/options/flake-parts-easyoverlay
        inputs.flake-parts.flakeModules.easyOverlay
        # Cachix pre-commit hooks: https://github.com/cachix/pre-commit-hooks.nix
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.hercules-ci-effects.flakeModule

        ./nixos/flake-module.nix
      ];

      # Expose private flake values to the repl for inspection
      debug = true;

      herculesCI = {
        onSchedule.default.when = {
          hour = 0;
          minute = 5;
        };
      };

      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', ... }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        pre-commit.settings.hooks.nixpkgs-fmt.enable = false;

        treefmt = {
          projectRootFile = "flake.nix";
          # Use as the flake's 'nix fmt' formatter
          flakeFormatter = true;
          programs = {
            shfmt.enable = true;
            shellcheck.enable = true;
            nixpkgs-fmt.enable = true;
            black.enable = true;
          };
          settings = {
            global.excludes = [ ];
          };
        };

        # Attributes to add to overlays.default
        overlayAttrs = {
          inherit (config.packages) figlet;
        };

        # Packages
        packages.figlet = inputs'.nixpkgs.legacyPackages.figlet;
        packages.flake-benchmark =
          let inherit (inputs'.nixpkgs.legacyPackages) runCommand nix hyperfine;
            # Command to benchmark
            check = "${nix}/bin/nix flake check ${self}";
            # Run the check command first to ensure the evaluation cache is populated
            # and necessary flake packages are built.
            benchmark = "${hyperfine}/bin/hyperfine --prepare '${check}' '${check}'";
          in
          runCommand "flake-benchmark" { } benchmark;

        # Applications
        apps.figlet = {
          type = "app";
          program = inputs'.nixpkgs.legacyPackages.figlet;
        };

        # devShells.default = { };

        checks.figlet = inputs'.nixpkgs.legacyPackages.figlet;

        # Format with nixpkgs-fmt
        formatter = inputs'.nixpkgs.legacyPackages.nixpkgs-fmt;
      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.

        nixosConfigurations = { };
        nixosModules = { };
      };
    };

  # Automatic nix.conf settings (accepted automatically when 'accept-flake-config = true')
  nixConfig.extra-experimental-features = "nix-command flakes";
  nixConfig.extra-substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://cfeeley.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "cfeeley.cachix.org-1:b+RrHsy/4WWys2o6T4YyF66OhdiZUF/R/N46JcS0HJU="
  ];
}
