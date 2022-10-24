{ config
, lib
, pkgs
, ...
}:
{
  homebrew.masApps = {
    "Vimari" = 1480933944; # Safari port of Vimium
    "AdGuard for Safari" = 1440147259; # Not quite uBlock origin
  };
}
