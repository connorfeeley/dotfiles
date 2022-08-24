final: prev: {
  pwrbar = final.callPackage ./pwrbar {
    python-kasa = final.python3Packages.python-kasa;
  };
  hpi = final.callPackage ./HPI {};
  orgparse = final.callPackage ./orgparse {};
  promnesia = final.callPackage ./promnesia {
    inherit (final) hpi orgparse;
  };
}
