{ lib, pkgs, ... }:
lib.mkMerge [{ home.packages = with pkgs; [ distrobox arion ]; }]
