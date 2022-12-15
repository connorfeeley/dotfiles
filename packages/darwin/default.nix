final: _prev: {
  macports = final.darwin.apple_sdk_11_0.callPackage ./macports.nix { };
  installApplication = final.darwin.apple_sdk_11_0.callPackage ./installApplication.nix { };
}
