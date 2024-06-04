{ config, lib, pkgs, ... }:
lib.mkIf (!config.nixos-vm.enable) {
  boot.kernelModules = [ "kvm-amd" ];
  boot.initrd.kernelModules = [ "kvm-amd" ];
  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Driver that exposes access to the SMU (System Management Unit) for certain AMD Ryzen Processors
  boot.extraModulePackages = [
    (pkgs.nur.repos.arc.packages.ryzen-smu.override {
      linux = config.boot.kernelPackages.kernel;
    })
  ];

  programs.ryzen-monitor-ng.enable = true;
}
