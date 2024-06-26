{ collective }:
with collective.hmArgs.profiles;
let
  ###: --- roles ---------------------------------------------------------------

  # SSH and shells
  shell = [ shells.zsh shells.fish shells.bash shells.nushell ssh ];

  # beep boop
  developer = [
    direnv
    vim
    # jetbrains
    git
    shells.zsh
    shells.bash
    shells.fish
    kitty
    ssh
    ops # nixops
    development.navi
    development.tools
    development.vscode
    python
    virtualisation.common
    ranger
    tealdeer
    vim
  ];

  emacs-config = [ direnv emacs git development.tools ];

  # Desktop packages
  graphical = [
    desktop.common
    desktop.gnome
    desktop.gui
    desktop.word-processing
    firefox
    keyboard
    misc
    themes
  ];

  # Slightly more frivolous packages
  personalised = [
    shells.fish
    misc
    newsboat
    obs-studio
    spotify
    zotero
    mpv
    media
  ];

  # Someone else's computer
  server = [ ssh ];

  # If I told you then I'd have to kill you
  trusted = [
    gpg
    gpg-unlock
    mail
    secrets.common
    secrets.password-store
    yubikey
    ledger
  ];

  # Web development :(
  webdev = [
    aws
    # nodejs
  ];

  # ∨ ¬ ∧
  fpgadev = [ development.xilinx ];

  # Tools for network diagnostics, information gathering, reverse engineering, etc.
  security = [ security-tools ];

  # OS-specific: Linux-only
  linux = [ nixos.rofi nixos.development.tools virtualisation.docker ];

  # OS-specific: MacOS
  macos = [ virtualisation.docker darwin.search /*darwin.dock-apps*/ ];

  ###: --- meta-roles ----------------------------------------------------------
  roles = {
    inherit shell developer emacs-config graphical personalised server trusted
      webdev fpgadev security linux macos;

    workstation = shell ++ developer ++ graphical ++ trusted ++ webdev
      ++ fpgadev ++ security;
  };
in
roles
