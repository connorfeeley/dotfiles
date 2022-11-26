{ inputs, self, nixpkgs }: {
  meta = let
    defaultOverlays = [
      self.overlays.default
      inputs.nixos-cn.overlay
    ];
    config.allowUnfree = true;
    x86_64-nixpkgs = import nixpkgs {
      system = "x86_64-linux";
      inherit config;
      overlays = defaultOverlays ++ [(final: prev: {
        nbfc-linux = inputs.nbfc-linux.defaultPackage.x86_64-linux;
      })];
    };
    aarch64-nixpkgs = import nixpkgs {
      system = "aarch64-linux";
      inherit config;
      overlays = defaultOverlays ++ [(final: prev: {
        carinae = inputs.carinae.packages.aarch64-linux.default;
      })];
    };
  in {
    nixpkgs = x86_64-nixpkgs;
    nodeNixpkgs = {
      rpi = aarch64-nixpkgs;
      cindy = aarch64-nixpkgs;
    };
  };
  defaults.deployment = {
    buildOnTarget = true;
    #replaceUnknownProfiles = false;
    allowLocalDeployment = true;
  };
} // builtins.mapAttrs (name: value: {
  nixpkgs.system = value.config.nixpkgs.system;
  imports = value._module.args.modules;
}) self.nixosConfigurations
