final: _prev: {
  aranet4 = final.callPackage ./aranet4 { };
  pwrbar = final.callPackage ./pwrbar {
    python-kasa = final.python3Packages.python-kasa;
  };
  hpi = final.callPackage ./HPI { };
  orgparse = final.callPackage ./orgparse { };
  promnesia = final.callPackage ./promnesia { inherit (final) hpi orgparse; };
  hdl_checker = final.callPackage ./hdl_checker {
    inherit (final.inputs.mach-nix.lib.${final.system}) buildPythonApplication;
  };
}
