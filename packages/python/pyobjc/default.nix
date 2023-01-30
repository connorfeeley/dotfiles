{ lib
, stdenv
, python3Packages # WARNING: may be broken on python38! "offsetof(....., vectorcall)" or something
, fetchFromGitHub
, libffi
, darwin
, frameworks
, callPackage
, xcbuild
, xcbuildHook
}:
let
  mkPackage = { pname, pythonImportsCheck, pytestFlagsArray, disabledTestPaths, disabledTests, doCheck ? false, extraBuildInputs ? [ ], frameworkInputs ? [ ] }:
    callPackage ./generic.nix {
      inherit pname pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck extraBuildInputs frameworkInputs;

      inherit lib stdenv python3Packages fetchFromGitHub libffi darwin frameworks xcbuild xcbuildHook;
    };
in
rec {
  # Install order ():
  # - pyobjc-core
  # - pyobjc-framework-Cocoa
  # - pyobjc-framework-Quartz

  # nix-build --pure ~dots -A packages.aarch64-darwin.pyobjc.pyobjc-core --show-trace
  pyobjc-core = mkPackage {
    pname = "pyobjc-core";
    pythonImportsCheck = [ "objc._objc" ];
    doCheck = true;
    pytestFlagsArray = [ "PyObjCTest/" ];
    disabledTestPaths = [
      "PyObjCTest/test_vectorcall.py"
      "PyObjCTest/test_set_interface.py"
      "PyObjCTest/test_dict_interface.py"
      "PyObjCTest/test_array_interface.py"
      "PyObjCTest/test_archive_python.py"
      # Tries to assert that a path is equal to the user's home - would break purity
      "test_bundleFunctions.py"
    ];
    disabledTests = [
      "PyObjCTest.test_transform"
      # Tries to assert that a path is equal to the user's home - would break purity
      "PyObjCTest.test_bundleFunctions.TestBundleFunctions"
      # TODO: test_interop_int (PyObjCTest.test_archiving_secure_interop.TestNSKeyedArchivingInterop) ... 2023-01-31 02:52:55.243 dump-nsarchive-securecoding[75588:2670416] Cannot decode archive: Error Domain=NSCocoaErrorDomain Code=4865 "requested key: 'root'" UserInfo={NSDebugDescription=requested key: 'root'}
    ];
    frameworkInputs = [ darwin.apple_sdk.objc4 ];
    extraBuildInputs = [ darwin.DarwinTools ];
  };
  pyobjc-framework-Cocoa = mkPackage {
    pname = "pyobjc-framework-Cocoa";
    pythonImportsCheck = [ "objc.Cocoa" ];
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
    extraBuildInputs = [ pyobjc-core ];
    frameworkInputs = with frameworks; [ Cocoa ];
  };
  pyobjc-framework-Quartz = mkPackage {
    pname = "pyobjc-framework-Quartz";
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
    extraBuildInputs = [ pyobjc-core ];
    frameworkInputs = with frameworks; [ Quartz ImageCaptureCore ];
  };
  # pyobjc-framework-Quartz = callPackage ./Quartz.nix { };
}
