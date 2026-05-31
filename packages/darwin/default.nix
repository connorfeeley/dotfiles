final: _prev: {
  macports = final.callPackage ./macports.nix { };

  installApplication =

    final.callPackage ./installApplication.nix { };
}
