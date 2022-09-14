{ config
, lib
, pkgs
, ...
}:

{
  home.packages = with pkgs; [
    ## === Reverse Engineering ===
    (binwalk.override { visualizationSupport = true; })

    ## === Local Development ===

    act # Run GitHub Actions locally
  ];
}
