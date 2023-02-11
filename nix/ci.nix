{ self, lib, inputs, ... }: {
  imports = [
    # Cachix pre-commit hooks: https://github.com/cachix/pre-commit-hooks.nix
    inputs.pre-commit-hooks-nix.flakeModule
    inputs.treefmt-nix.flakeModule
    inputs.hercules-ci-effects.flakeModule
  ];
  herculesCI = {
    onSchedule.default.when = {
      hour = 0;
      minute = 5;
    };
  };

  perSystem = { config, self', inputs', pkgs, ... }: {
    # Definitions like this are entirely equivalent to the ones
    # you may have directly in flake.nix.
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
      settings.global.excludes = [ ];
    };

    checks.figlet = inputs'.nixpkgs.legacyPackages.figlet;

    # Format with nixpkgs-fmt
    formatter = inputs'.nixpkgs.legacyPackages.nixpkgs-fmt;
  };
  flake = {
    nixosModules = { };
  };
}
