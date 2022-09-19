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

    ## === Networking ===
    arping
    nmap
    ncrack
    wpscan
    tcpreplay
    tcpdump
    wireshark
    sslsplit
  ] ++ (lib.optionals pkgs.stdenv.isLinux [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = true; })
    radare2
    radare2-cutter

    ## === Networking ===
    aircrack-ng
    mitmproxy
    proxychains
    udptunnel
    macchanger
    spike # Network protocol fuzzer

    ## === Information Gathering ===
    theharvester

    ## === Passwords ===
    thc-hydra
    john
    hashcat
    hashcat-utils
  ]);
}
