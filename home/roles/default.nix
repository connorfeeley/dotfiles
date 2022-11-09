{ profiles }:
with profiles; let
  shell = [
    shells.zsh
    shells.fish
    shells.bash
    ssh
  ];

  developer = [
    shells.fish
    direnv
    emacs
    vim
    git
    shells.zsh
    shells.bash
    shells.fish
    ssh
    ops # nixops
    development.tools
    virtualisation.common
  ];

  graphical = [
    desktop.common
    # desktop.gnome
    desktop.gui
    desktop.word-processing
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
    shells.fish
    misc
    newsboat
    obs-studio
    spotify
    zotero

    nnn
    ranger
    tealdeer
    vim
  ];

  remote = [
    ops
    ssh
  ];

  trusted = [
    gpg
    mail
    secrets.common
    secrets.password-store
    yubikey
    work
    sync
  ];

  webdev = [
    aws
    # nodejs
  ];

  fpgadev = [
    development.xilinx
  ];

  security = [
    security-tools
  ];

  linux = [
    desktop.xmonad
    nixos.development.tools
    virtualisation.docker
  ];

  macos = [
    virtualisation.podman

    darwin.search
  ];

  roles = {
    inherit
      shell
      developer
      graphical
      personalised
      remote
      trusted
      webdev
      fpgadev
      security
      linux
      macos
      ;

    workstation =
      shell
      ++ developer
      ++ graphical
      ++ personalised
      ++ trusted
      ++ webdev
      ++ fpgadev
      ++ security;
  };
in
roles
