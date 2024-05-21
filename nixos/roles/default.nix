{ collective }:
let
  graphical = (with collective.profiles; [
    fonts.common
    networking.ssh-host
    secrets
  ]) ++ (with collective.nixosProfiles; [
    desktop.common
    desktop.ddc-monitor-control
    video
    yubikey
    networking.avahi
  ]);

  server = (with (collective.profiles); [
    networking.common
    networking.tailscale
    networking.ssh-host
  ]) ++ (with collective.nixosProfiles; [ networking.geoip ]);

  tangible =
    (with (collective.profiles); [ networking.common networking.tailscale ])
    ++ (with collective.nixosProfiles; [
      audio
      bluetooth
      networking.wifi
      /* printers-scanners */
    ]);

  virt = with collective.nixosProfiles; [
    virtualisation.libvirtd
    virtualisation.docker
    virtualisation.podman
    virtualisation.vagrant
    # virtualisation.virtualbox
  ];

  fpgadev = with collective.nixosProfiles; [ fpga.intel-altera fpga.xilinx ];
in
{ inherit graphical server tangible virt fpgadev; }
