{ inputs
, config
, lib
, pkgs
, ...
}:
{
  imports = [ inputs.nix-serve-ng.nixosModules.default ];
  services.nix-serve = {
    enable = true;
    secretKeyFile = "/etc/nix/cache-priv-key.pem";
  };
}
