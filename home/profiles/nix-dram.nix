{ pkgs
, ...
}:

{
  #: Nix for lazy people; uses default-flake for 'nix <cmd> <default-flake>#' commands
  # NOTE: required to avoid strange lib.string errors when evaluating 'default-flake' option
  nix.package = pkgs.nix-dram;

  # Set 'nixpkgs' to be the default flake used by nix-dram tools
  nix.extraOptions = ''
    default-flake = nixpkgs
  '';

  home.packages = with pkgs; [
    nix-search #: pretty 'nix search <default-flake>#' interface
    nix-nar-listing #: unclear what exactly this is for

    # NOTE: marked broken upstream
    # nix-dram-progress
  ];
}
