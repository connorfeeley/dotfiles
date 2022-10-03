channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-stable)
    zathura
    wally-cli
    ;

  # sourcetrail = (channels.nixos-stable-21-11.sourcetrail.override { llvmPackages = channels.nixos-unstable.llvmPackages_13; }).overrideAttrs
  #   (previousAttrs: rec {
  #     src = prev.fetchFromGitHub {
  #       owner = "petermost";
  #       repo = "Sourcetrail";
  #       rev = "37b6d60e6e5ecb4b93d2660d2f718a1d8f8bed77";
  #       sha256 = "sha256-hNcqrrQFU+bxz4zp3Whh/zxvmeUZQvlaZkagvA4ywg4=";
  #     };
  #   });
}
