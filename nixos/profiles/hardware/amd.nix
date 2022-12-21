{ config
, lib
, ...
}: lib.mkIf (!config.nixos-vm.enable) {
  boot.kernelModules = [ "kvm-amd" ];
  boot.initrd.kernelModules = [ "kvm-amd" ];
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
