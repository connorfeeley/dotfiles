{ lib, stdenv
, python3Packages # WARNING: may be broken on python38! "offsetof(....., vectorcall)" or something
, fetchFromGitHub, libffi, darwin, frameworks, callPackage, xcbuild, xcbuildHook }:
let
  mkPackage = { pname, pythonImportsCheck, pytestFlagsArray, disabledTestPaths, disabledTests, doCheck ? false }:
    callPackage ./generic.nix {
      inherit pname pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck;

      inherit lib stdenv python3Packages fetchFromGitHub libffi darwin frameworks xcbuild xcbuildHook;
    };
in
{
  pyobjc-core = mkPackage {
    pname = "pyobjc-core";
    pythonImportsCheck = [ "objc._objc" ];
    doCheck = false;
    pytestFlagsArray = [ "PyObjCTest/" ];
    disabledTestPaths = [
      "PyObjCTest/test_vectorcall.py"
      "PyObjCTest/test_set_interface.py"
      "PyObjCTest/test_dict_interface.py"
      "PyObjCTest/test_array_interface.py"
      "PyObjCTest/test_archive_python.py"
    ];
    disabledTests = [ "PyObjCTest.test_transform" ];
  };
  # pyobjc-framework-Quartz = callPackage ./Quartz.nix { };
}
