collective: { inputs, ... }:
let
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin x86_64-darwin;
  inherit (digga.lib) importHosts importExportableModules rakeLeaves;

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  darwinModules = importExportableModules ./modules;
  profiles = rakeLeaves ./profiles;
  roles = import ./roles { inherit collective profiles; };

  importables = { inherit collective profiles roles primaryUser; };
in
{
  inherit importables;

  imports = [ (importHosts ./machines) ];

  hosts.MacBook-Pro = {
    system = aarch64-darwin;
    modules = with roles; workstation ++
      [ ];
  };

  hostDefaults = {
    system = aarch64-darwin;
    channelName = "nixpkgs-darwin";
    imports = [ collective.modules darwinModules ];
    modules = [
      collective.profiles.core
      profiles.core
      home-manager.darwinModules.home-manager
      digga.nixosModules.nixConfig

      # `nixosModules` is correct, even for darwin
      agenix.nixosModules.age
    ];
  };
}
