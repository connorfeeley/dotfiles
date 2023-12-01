{ lib, pkgs, ... }: {
  environment.systemPackages = [ pkgs.darwin.linux-builder ];
  nix.settings = {
    builders = lib.mkForce [
      "ssh-ng://builder@linux-builder aarch64-linux /etc/nix/builder_ed25519 8 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo="
    ];
    builders-use-substitutes = true;
  };
}
