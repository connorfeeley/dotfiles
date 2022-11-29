{ config
, lib
, pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    quartus-prime-lite
  ];
}
