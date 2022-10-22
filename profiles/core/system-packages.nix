{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib) dotfield;

  # TODO: add this via gitignore.nix or something to avoid IFD
  dotfieldScript =
    pkgs.writeScriptBin "dotfield"
    (builtins.readFile "${dotfield.srcPath}/packages/dotfield");
in {
  environment.systemPackages = with pkgs; [
    dotfieldScript
    hlissner-hey

    ## === Essentials ===
    bashInteractive
    bat
    binutils
    cacert
    coreutils-full
    exa
    fd
    findutils
    gawk
    git
    gnumake
    gnupg
    gnused
    gnutar
    grc
    jq
    less
    openssh
    openssl
    ripgrep
    rsync
    screen
    tmux
    vim

    ## === Network ===
    curl
    nmap
    wget
    whois
  ];
}
