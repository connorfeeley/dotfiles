{ config, options, lib, pkgs, inputs', ... }:
let
  inherit (config.boot.kernelPackages) nvidiaPackages;

  inherit (inputs'.nvidia-patch.packages) nvidia-patch;

  nvPackage = (nvidia-patch.override {
    nvidia_x11 = nvidiaPackages.stable; # FIXME: only using vulkan_beta since it satisfies nvidia_x11 being < v430
  }).overrideAttrs (old: {
    meta.broken = false;
  });

  xorgPackages = with pkgs.xorg; [ xhost xauth xinit xeyes ];
in
lib.mkIf (!options.virtualisation ? qemu) {
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  # NOTE: The lib.dotfiles.sys.hasNvidia function from lib/system/default.nix is equal to
  #       'hardware.nvidia.package != null'.
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    package = nvidiaPackages.production;
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;

    open = false;

    nvidiaSettings = true; # Enable nvidia-settings utility
    nvidiaPersistenced = false; # Don't run daemon to keep GPU state alive

    # "Whether to force-enable the full composition pipeline. This sometimes fixes
    # screen tearing issues. This has been reported to reduce the performance of
    # some OpenGL applications and may produce issues in WebGL. It also drastically
    # increases the time the driver needs to clock down after load."
    forceFullCompositionPipeline = false;
  };

  boot.plymouth = {
    enable = false;
    font = "${pkgs.ttc-subway}/share/fonts/truetype/${pkgs.ttc-subway.passthru.regular}.hardware";
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ vaapiVdpau ];

    driSupport = true;
    driSupport32Bit = pkgs.stdenv.isx86_64;
  };

  # Work around black screen issue on boot: https://github.com/NixOS/nixpkgs/issues/295218
  boot.initrd.kernelModules = [
    "nvidia"
    "nvidia_modeset"
    "nvidia_uvm"
    "nvidia_drm"
  ];

  virtualisation.docker.enableNvidia = config.virtualisation.docker.enable;
  virtualisation.containers.cdi.dynamic.nvidia.enable = config.virtualisation.podman.enable;

  environment.systemPackages = [ pkgs.nvtop pkgs.ddcutil pkgs.cudatoolkit config.boot.kernelPackages.nvidia_x11 ] ++ xorgPackages;
}
