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

  # FIXME: fpga.xilinx pulls in nix-xilinx, which still references the renamed
  # `pkgs.buildFHSUserEnvBubblewrap` (`buildFHSEnvBubblewrap` since 26.05). Its
  # `pkgs` is bound to bare `nixpkgs.legacyPackages.*`, so an overlay alias
  # can't reach it. Re-enable after updating nix-xilinx upstream.
  fpgadev = with collective.profiles.nixos; [ fpga.intel-altera ];
in
{ inherit graphical server tangible virt fpgadev; }
