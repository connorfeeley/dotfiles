{ collective }:
let
  graphical = (with collective.profiles.global; [
    fonts.common
    networking.ssh-host
    secrets
  ]) ++ (with collective.profiles.nixos; [
    desktop.common
    desktop.ddc-monitor-control
    video
    yubikey
    networking.avahi
  ]);

  server = (with (collective.profiles.global); [
    networking.common
    networking.tailscale
    networking.ssh-host
  ]) ++ (with collective.profiles.nixos; [ networking.geoip ]);

  tangible =
    (with (collective.profiles.global); [ networking.common networking.tailscale ])
    ++ (with collective.profiles.nixos; [
      bluetooth
      networking.wifi
      /* printers-scanners */
    ]);

  virt = with collective.profiles.nixos; [
    virtualisation.libvirtd
    virtualisation.docker
    virtualisation.podman
    virtualisation.vagrant
    # virtualisation.virtualbox
  ];

  fpgadev = with collective.profiles.nixos; [ fpga.intel-altera fpga.xilinx ];
in
{ inherit graphical server tangible virt fpgadev; }
