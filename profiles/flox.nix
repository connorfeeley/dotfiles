{ config
, lib
, pkgs
, ...
}:
# Flox: multi-platform and reproducible environment manager.
#   https://floxdev.com
# KLUDGE: disabled profile
lib.mkIf false {
  # NOTE: also requires adding flox's private GitHub token to nix.conf.
  nix.settings = {
    trusted-substituters = [ "https://cache.floxdev.com?trusted=1" ];
    trusted-public-keys = [ "flox-store-public-0:8c/B+kjIaQ+BloCmNkRUKwaVPFWkriSAd0JJvuDu4F0=" ];
  };

  environment.systemPackages = with pkgs; [
    # FIXME: bah. Needs to be impure.
    # Just install it in a profile, I guess.
    # flox
  ];
}
