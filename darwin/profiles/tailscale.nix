{ config
, lib
, pkgs
, ...
}:
{
  services.tailscale = {
    enable = false;
    magicDNS.enable = true;
    domain = "connorfeeley.github";
  };
}
