{ config
, lib
, pkgs
, ...
}:
# Flox: multi-platform and reproducible environment manager.
#   https://floxdev.com
{
  nix.settings = {
    trusted-substituters = [ "https://cache.floxdev.com?trusted=1" ];
    trusted-public-keys = [ "flox-store-public-0:8c/B+kjIaQ+BloCmNkRUKwaVPFWkriSAd0JJvuDu4F0=" ];
  };
}
