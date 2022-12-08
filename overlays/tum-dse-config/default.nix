{ inputs }:
final: prev:
let
  packagesFrom = inputAttr: inputAttr.packages.${final.system};
in
{
  __dontExport = true;

  inherit (packagesFrom inputs.tum-dse-config)
    netboot
    netboot-pixie-core
    sfc-drivers
    xilinx-cable-drivers
    xilinx-env
    xilinx-firmware
    xrt
    xrt-drivers
  ;
}
