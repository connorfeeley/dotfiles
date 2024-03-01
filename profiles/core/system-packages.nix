{ config, lib, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    ## === Essentials ===
    bashInteractive
    bat
    binutils
    cacert
    coreutils-full
    eza
    fd
    dua
    duf
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
    vim

    ## === Network ===
    curl
    nmap
    wget
    # whois
  ];

  environment.shellAliases = {
    ll = "${pkgs.eza} -l";
    rgi = "${pkgs.ripgrep} --no-ignore";
    fdi = "${pkgs.fd} --no-ignore";
  };
}
