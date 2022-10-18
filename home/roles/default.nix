{profiles}:
with profiles; let
  developer = [
    direnv
    emacs
    git
    shells.zsh
    shells.bash
    shells.fish
    ssh
    virtualisation.docker
    nix-dram
  ];

  graphical = [
    desktop.common
    # desktop.gnome
    desktop.xmonad
    desktop.gui
    firefox
    chromium
    foot
    keyboard
    kitty
    misc
    mpv
    themes
  ];

  personalised = [
    misc
    newsboat
    obs-studio
    spotify
    zotero
  ];

  remote = [
    shells.fish
    ssh
  ];

  trusted = [
    gpg
    mail
    secrets.password-store
    yubikey
    work
  ];

  webdev = [
    aws
    nodejs
  ];

  fpgadev = [
    development.xilinx
  ];

  security = [
    security-tools
  ];

  macos = [
    virtualisation.podman

    darwin.search
  ];

  hm-only = [
    generic-linux
  ];

  roles = {
    inherit
      developer
      graphical
      personalised
      remote
      trusted
      webdev
      fpgadev
      security
      macos
      hm-only
      ;

    workstation =
      developer
      ++ graphical
      ++ personalised
      ++ trusted
      ++ webdev
      ++ fpgadev
      ++ security;
  };
in
  roles
