{ self, lib, inputs, ... }: {
  imports = [ ./ci.nix ];
  perSystem = { config, self', inputs', pkgs, ... }: {
    # Definitions like this are entirely equivalent to the ones
    # you may have directly in flake.nix.
  };
  flake = { };
}
