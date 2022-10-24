{ config
, lib
, pkgs
, ...
}:
{
  services.tailscale = {
    enable = true;
    magicDNS.enable = true;
    domain = "connorfeeley.github";
  };
}
