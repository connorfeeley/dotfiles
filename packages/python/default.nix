final: _prev: {
  aranet4 = final.callPackage ./aranet4 { };
  pwrbar = final.callPackage ./pwrbar { };
  orgparse = final.callPackage ./orgparse { };
  promnesia = final.callPackage ./promnesia { inherit (final) hpi orgparse; };
}
