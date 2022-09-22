{ config
, lib
, pkgs
, ...
}:
let
    inherit (pkgs.stdenv) isLinux;
    inherit (pkgs.stdenv) isDarwin;
    inherit (pkgs.stdenv) isAarch64;
in {
  home.packages = with pkgs; [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = isLinux; })

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
    socat
    tcpreplay
    tcpdump
    wireshark
    sslsplit
  ] ++ (lib.optionals (isLinux && !isAarch64) [
    ## === Reverse Engineering ===
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
