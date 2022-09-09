{ config
, lib
, pkgs
, ...
}:

{
  home.packages = with pkgs; [
    # Via nix-xilinx
    vivado
    vitis
    xilinx-shell

    xilinx-bootgen
    jtag-remote-server
  ];

  # FIXME: not portable; workstation machine only
  xdg.configFile."xilinx/nix.sh".text = ''
    INSTALL_DIR=/mnt/ssd/xilinx/vivado
    # The directory in which there's a /bin/ directory for each product, for example:
    # $HOME/downloads/software/xilinx/Vivado/2022.1/bin
    VERSION=2021.2
  '';
}
