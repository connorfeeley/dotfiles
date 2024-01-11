# SPDX-FileCopyrightText: 2023 Connor Feeley
#
# SPDX-License-Identifier: BSD-3-Clause

# Create .nixd.json file with:
#   nix eval --json --file .nixd.nix > .nixd.json

{
  eval = {
    # Example target for writing a package.
    # target = {
    #   args = [ "--expr" "with import <nixpkgs> { }; callPackage ./default.nix { }" ];
    #   installable = "";
    # };
    # Example target for system (using flake-compat)
    target = {
      args = [ "-f" "default.nix" ];
      installable = "nixosConfigurations.workstation";
    };
    # Force thunks
    depth = 10;
    # Specify number of threads to use. Default: std::thread::hardware_concurrency()
    # workers = 5;
  };
  formatting.command = "nixpkgs-fmt";
  # Specify which option set to use.
  options = {
    enable = true;
    target = {
      args = [ ];
      # Example installable for flake-parts, nixos, and home-manager

      # flake-parts
      # installable = "/flakeref#debug.options";

      # nixOS configuration
      installable = ".#nixosConfigurations.workstation.options";

      # home-manager configuration
      # installable = "/flakeref#homeConfigurations.cfeeley.options";
    };
  };
}
