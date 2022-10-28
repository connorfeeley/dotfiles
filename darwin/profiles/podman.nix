{ config
, lib
, pkgs
, ...
}:
let
  inherit (pkgs.stdenv) isDarwin;
  inherit (config.lib.dag) entryAfter;
  inherit (config.xdg) configHome;

  qemuPackage = pkgs.qemu; # or pkgs.qemu_kvm

  # TODO: Should perhaps match all podman-machine-*.json files.
  podmanMachineConfDir = "${configHome}/containers/podman/machine/qemu";
in
lib.mkMerge [
  {
    homebrew = {
      brews = [ "podman" ];
      casks = [ "podman-desktop" ];
    };

    environment.variables = {
      DOCKER_HOST = "unix://$XDG_DATA_HOME/containers/podman/machine/podman-machine-default/podman.sock";
    };
  }

  # Podman needs qemu, but the default machine isn't able to locate the right binaries by default.
  # (lib.mkIf isDarwin {
  #   home.packages = lib.optionals isDarwin [ qemuPackage ];

  #   home.activation.fixPodmanMachineConf =
  #     let
  #       sed = "$DRY_RUN_CMD ${pkgs.gnused}/bin/sed";
  #       newPath = "${qemuPackage}/share/qemu/edk2-aarch64-code.fd";
  #     in
  #     entryAfter [ "writeBoundary" ] ''
  #       if [[ -f "${podmanMachineConfDir}/podman-machine-default.json" ]]; then
  #         echo "Podman: updated edk2-aarch64-code.fd path to ${newPath}"
  #         ${sed} -i 's#file=.*edk2-aarch64-code.fd#file=${newPath}#g' \
  #           ${podmanMachineConfDir}/podman-machine-default.json
  #       else
  #         echo "Podman: run 'podman machine init' to initialize podman"
  #       fi
  #     '';
  # })
]
