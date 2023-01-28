{ collective
, profiles
,
}:
let
  graphical =
    (with collective.profiles; [
      fonts.common
      networking.ssh-host
      secrets
      # fonts.pragmatapro # TODO: remove
    ])
    ++ (with profiles; [
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
  ]) ++ (with profiles; [ ]);

  tangible =
    (with (collective.profiles); [
      networking.common
      networking.tailscale
    ])
    ++ (with profiles; [
      audio
      bluetooth
      printers-scanners
      networking.wifi
    ]);

  virt = with profiles; [
    virtualisation.libvirtd
    virtualisation.podman
    virtualisation.vagrant
    # virtualisation.virtualbox
  ];

  fpgadev = with profiles; [
    fpga.intel-altera
    fpga.xilinx
  ];
in
{
  inherit
    graphical
    server
    tangible
    virt
    fpgadev
    ;
}
