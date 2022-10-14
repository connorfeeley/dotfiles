{ config
, lib
, pkgs
, ...
}:

{
  home.packages = with pkgs; [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = pkgs.stdenv.isLinux; })

    ## === Crypto-Stego ===
    ccrypt

    ## === Forensics ===
    chkrootkit
    ddrescue
    exiv2
    pdf-parser
    bingrep # Greps through binaries from various OSs and architectures, and colors them.

    ## === Networking ===
    arping
    nmap
    ncrack
    wpscan
    socat
    tcpreplay
    tcpdump
    wireshark
    sslsplit
    rustscan
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = true; })
    radare2
    cutter

    ## === Networking ===
    aircrack-ng
    mitmproxy
    proxychains
    udptunnel
    macchanger
    spike # Network protocol fuzzer

    ## === Information Gathering === (see: https://github.com/fabaff/nix-security-box/blob/main/information-gathering.nix)
    cloudbrute
    maigret
    metabigor
    p0f
    sn0int #: semi-automatic OSINT framework and package manager
    urlhunter #: a recon tool that allows searching on URLs that are exposed via shortener services
    socialscan #: CLI for querying username and email usage on online platforms
    theharvester #: E-mails, subdomains and names harvester
    poppler_utils # pdf rendering tools

    ## === Passwords ===
    thc-hydra
    john
    hashcat
    hashcat-utils
  ]);
}
