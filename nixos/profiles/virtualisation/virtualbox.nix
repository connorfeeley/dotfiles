{ lib, pkgs, ... }:
let inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in lib.mkMerge [
  (lib.mkIf isLinux {
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableWebService = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;
  })
  (lib.mkIf isDarwin {
    environment.systemPackages = with pkgs; [ dnsmasq ];
    services.dnsmasq = {
      enable = false;
    };
  })
]
