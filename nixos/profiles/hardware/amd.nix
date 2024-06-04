{ config, lib, pkgs, ... }:
lib.mkIf (!config.nixos-vm.enable) {
  boot.kernelModules = [ "kvm-amd" ];
  boot.initrd.kernelModules = [ "kvm-amd" ];
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  programs.ryzen-monitor-ng.enable = true;

  # Driver that exposes access to the SMU (System Management Unit) for certain AMD Ryzen Processors
  hardware.cpu.amd.ryzen-smu.enable = true;
}
