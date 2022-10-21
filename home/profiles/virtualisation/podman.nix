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
    home.packages = with pkgs; [ podman podman-compose ];
  }

  # Podman needs qemu, but the default machine isn't able to locate the right binaries by default.
  {
    home.packages = lib.optionals isDarwin [ qemuPackage ];

    home.activation.fixPodmanMachineConf =
      let
        sed = "$DRY_RUN_CMD ${pkgs.gnused}/bin/sed";
        newPath = "${qemuPackage}/share/qemu/edk2-aarch64-code.fd";
      in
      entryAfter [ "writeBoundary" ] ''
        if [[ -f "${podmanMachineConfDir}/pdoman-machine-default.json" ]]; then
          echo "Podman: updated edk2-aarch64-code.fd path to ${newPath}"
          ${sed} -i 's#file=.*edk2-aarch64-code.fd#file=${newPath}#g' \
            ${podmanMachineConfDir}/podman-machine-default.json
        else
          echo "Podman: updated edk2-aarch64-code.fd path to ${newPath}"
        fi
      '';
  }
]
