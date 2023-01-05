final: _prev: {
  macports = final.darwin.apple_sdk_11_0.callPackage ./macports.nix { };

  installApplication = final.darwin.apple_sdk_11_0.callPackage ./installApplication.nix { };

  input-leap = final.libsForQt5.callPackage ./input-leap {
    inherit (final.darwin.apple_sdk.frameworks) ApplicationServices Carbon Cocoa CoreServices ScreenSaver;
  };
}
