{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    # Via nix-xilinx
    vivado
    vitis
    xilinx-shell

    # Via nixpkgs
    xilinx-bootgen
    jtag-remote-server
  ];

  services.udev.extraRules =
    let ddc-rules = ''
      SUBSYSTEM=="i2c-dev", ACTION=="add",\
        ATTR{name}=="NVIDIA i2c adapter*",\
        TAG+="ddcci",\
        TAG+="systemd",\
        ENV{SYSTEMD_WANTS}+="ddcci@$kernel.service"
    '';
    in
    ''
      # For monitor brightness control
      ${ddc-rules}

      # ### 60-openocd.rules (partial) -- UDEV rules for Altera USB Blaster
      # # Altera USB Blaster
      # ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6001", MODE="660", GROUP="plugdev", TAG+="uaccess"
      # # Altera USB Blaster2
      # ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6010", MODE="660", GROUP="plugdev", TAG+="uaccess"
      # ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6810", MODE="660", GROUP="plugdev", TAG+="uaccess"

      ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="06ad", MODE="660", GROUP="plugdev", TAG+="uaccess"

      ### 52-digilent-usb.rules -- UDEV rules for Digilent USB Devices

      # Create "/dev" entries for Digilent device's with read and write
      # permission granted to all users.
      ATTR{idVendor}=="1443", MODE:="666"
      ACTION=="add", ATTR{idVendor}=="0403", ATTR{manufacturer}=="Digilent", MODE:="666"

      ### 52-xilinx-ftdi-usb.rules -- UDEV rules for Xilinx USB Devices

      # version 0001
      # Create "/dev" entries for Xilinx device's with read and write
      # permission granted to all users.
      ACTION=="add", ATTR{idVendor}=="0403", ATTR{manufacturer}=="Xilinx", MODE:="666"

      # The following rules (if present) cause UDEV to ignore all UEVENTS for
      # which the subsystem is "usb_endpoint" and the action is "add" or
      # "remove". These rules are necessary to work around what appears to be a
      # bug in the Kernel used by Red Hat Enterprise Linux 6/CentOS 5. The Kernel
      # sends UEVENTS to remove and then add entries for the endpoints of a USB
      # device in "/dev" each time a process releases an interface. This occurs
      # each time a data transaction occurs. When an FPGA is configured or flash
      # device is written a large number of transactions take place. If the
      # following lines are commented out then UDEV will be overloaded for a long
      # period of time while it tries to process the massive number of UEVENTS it
      # receives from the kernel. Please note that this work around only applies
      # to systems running RHEL6 or CentOS 5 and as a result the rules will only
      # be present on those systems.
      # version 0002
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0008", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0007", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0009", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="000d", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="000f", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0013", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0015", MODE="666"

      ### 52-xilinx-pcusb.rules
      # version 0002
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0008", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0007", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0009", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="000d", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="000f", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0013", MODE="666"
      ATTR{idVendor}=="03fd", ATTR{idProduct}=="0015", MODE="666"
    '';

}
