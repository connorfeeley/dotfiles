{ config
, options
, lib
, pkgs
, ...
}:
let
  inherit (config.boot.kernelPackages) nvidiaPackages;

  nvStable = nvidiaPackages.stable;

  xorgPackages = with pkgs.xorg; [ xhost xauth xinit xeyes ];
in
{
  nixpkgs.config.allowUnfree = lib.mkForce true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  # NOTE: The lib.dotfield.sys.hasNvidia function from lib/system/default.nix is equal to
  #       'hardware.nvidia.package != null'.
  hardware.nvidia.package = nvStable;
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia.modesetting.enable = false;

  # Prevent display corruption upon wake from a suspended or hibernated state.
  hardware.nvidia.powerManagement.enable = true;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ vaapiVdpau ];

    driSupport = true;
    # NOTE: When built as an aarch64-linux VM on an aarch64-darwin host,
    # complains that 32-bit DRI only makes sense on 64-bit systems.
    driSupport32Bit = lib.mkForce (!options.virtualisation ? qemu);
  };

  virtualisation.docker = {
    enableNvidia = true;
  };

  environment.systemPackages = with pkgs; [ nvtop ddcutil ] ++ xorgPackages;
}
