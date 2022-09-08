{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.boot.kernelPackages) nvidiaPackages;

  nvStable = nvidiaPackages.stable;
  nvBeta = nvidiaPackages.beta;
  nvLatest =
    if (lib.versionOlder nvBeta.version nvStable.version)
    then nvStable
    else nvBeta;
in {
  nixpkgs.config.allowUnfree = lib.mkForce true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  hardware.nvidia.package = nvLatest;
  services.xserver.videoDrivers = ["nvidia"];

  # Required for Wayland?
  # NOTE: The lib.dotfield.sys.hasNvidia function from lib/system/default.nix is equal to
  #       'nvidia.modesetting.enable'.
  hardware.nvidia.modesetting.enable = true;

  # Prevent display corruption upon wake from a suspended or hibernated state.
  hardware.nvidia.powerManagement.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = with pkgs; [vaapiVdpau];

  environment.systemPackages = with pkgs; [ nvtop xorg.xhost xorg.xinit ];
}
