# Table of Contents

1.  [Useful Commands](#useful-commands)
    1.  [Build `debian-vm` system (inside VM)](#build-debian-vm)
    2.  [`deploy-rs`: build `debian-vm` in QEMU](#deploy-rs-build)


<a id="useful-commands"></a>

# Useful Commands


<a id="build-debian-vm"></a>

## Build `debian-vm` system (inside VM)

    fnix build ~/.config/dotfield#homeConfigurationsPortable.aarch64-linux."cfeeley@debian-vm".activationPackage --show-trace

    ./result/bin/home-manager-generation


<a id="deploy-rs-build"></a>

## `deploy-rs`: build `debian-vm` in QEMU

    deploy --skip-checks .#debian-vm -- --print-build-logs --show-trace
