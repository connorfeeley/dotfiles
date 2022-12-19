{ config
, lib
, modulesPath
, pkgs
, ...
}: {
  disabledModules = [ "virtualisation/parallels-guest.nix" ];

  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ./parallels-guest.nix
  ];

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "prl-tools"
    ];

  hardware.parallels = {
    enable = true;
    package = config.boot.kernelPackages.callPackage ./prl-tools.nix { };
  };
}
