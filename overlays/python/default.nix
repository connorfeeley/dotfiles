{ inputs }:
channels: final: prev:
let packagesFrom = inputAttr: inputAttr.packages.${final.system};
in rec {
  pythonPackagesExtensions = prev.pythonPackagesExtensions
    ++ [ (python-final: python-prev: { }) ];
}
