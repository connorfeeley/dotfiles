{ config
, lib
, pkgs
, ...
}:
let
  inherit (config.boot.kernelPackages) nvidiaPackages;

  nvStable = nvidiaPackages.stable;

  xorgPackages = with pkgs.xorg; [ xhost xauth xinit xeyes ];
in
lib.mkIf (!config.nixos-vm.enable) {
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  # NOTE: The lib.dotfield.sys.hasNvidia function from lib/system/default.nix is equal to
  #       'hardware.nvidia.package != null'.
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    package = nvStable;
    modesetting.enable = false;
    nvidiaSettings = true; # Enable nvidia-settings utility
    nvidiaPersistenced = false; # Don't run daemon to keep GPU state alive
    # Prevent display corruption upon wake from a suspended or hibernated state.
    powerManagement.enable = true;

    # "Whether to force-enable the full composition pipeline. This sometimes fixes
    # screen tearing issues. This has been reported to reduce the performance of
    # some OpenGL applications and may produce issues in WebGL. It also drastically
    # increases the time the driver needs to clock down after load."
    forceFullCompositionPipeline = false;
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ vaapiVdpau ];

    driSupport = true;
    driSupport32Bit = pkgs.stdenv.isx86_64;
  };

  virtualisation.docker.enableNvidia = lib.mkIf config.virtualisation.docker.enable true;
  virtualisation.podman.enableNvidia = lib.mkIf config.virtualisation.docker.enable true;

  environment.systemPackages = with pkgs; [ nvtop ddcutil cudatoolkit linuxPackages.nvidia_x11 ] ++ xorgPackages;
}
