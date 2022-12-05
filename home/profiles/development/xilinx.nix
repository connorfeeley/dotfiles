{ pkgs
, ...
}:

{
  home.packages = with pkgs; [
  ];

  # FIXME: not portable; workstation machine only
  xdg.configFile."xilinx/nix.sh".text = ''
    INSTALL_DIR=/mnt/zfs/data/Xilinx/Vivado/2021.2
    # The directory in which there's a /bin/ directory for each product, for example:
    # $HOME/downloads/software/xilinx/Vivado/2022.1/bin
    VERSION=2021.2
  '';
}
