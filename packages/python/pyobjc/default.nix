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
  mkPackage =
    { pname
    , pythonImportsCheck
    , pytestFlagsArray ? [ ]
    , disabledTestPaths ? [ ]
    , disabledTests ? [ ]
    , doCheck ? false
    , extraBuildInputs ? [ ]
    , frameworkInputs ? [ ]
    , checkInputs ? [ ]
    , preCheck ? ""
    }:
    callPackage ./generic.nix {
      inherit pname pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck extraBuildInputs frameworkInputs preCheck checkInputs;

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

    # requires additional data
    preCheck = ''
      set -x
      # disable tests that check paths outside fo the sandbox
      substituteInPlace PyObjCTest/test_bundleFunctions.py \
        --replace 'self.assertEqual(value, os.path.expanduser("~"))' 'self.assertEqual(value, value)'
    '';
    disabledTestPaths = [
      # Skip the examples tests
      "Examples/GUITests/test_modalsession.py"
      "Examples/NonFunctional/RemotePyInterpreter/test_client.py"
      "Examples/NonFunctional/RemotePyInterpreter/AsyncPythonInterpreter.py"
      "Examples/NonFunctional/RemotePyInterpreter/ConsoleReactor.py"
      "Examples/NonFunctional/RemotePyInterpreter/netrepr.py"
      "Examples/NonFunctional/RemotePyInterpreter/remote_bootstrap.py"
      "Examples/NonFunctional/RemotePyInterpreter/remote_console.py"
      "Examples/NonFunctional/RemotePyInterpreter/remote_pipe.py"
      "Examples/NonFunctional/RemotePyInterpreter/RemotePyInterpreter.py"
      "Examples/NonFunctional/RemotePyInterpreter/setup.py"
      "Examples/NonFunctional/RemotePyInterpreter/tcpinterpreter.py"
      "Examples/NonFunctional/RemotePyInterpreter/test_client.py"

      # TODO: does this do anything?
      "PyObjCTest/test_bundleFunctions.py"
    ];
    disabledTests = [
      "PyObjCTest.test_bundleFunctions.TestBundleFunctions"
      "test_bundleFunctions.TestBundleFunctions"
      "PyObjCTest.test_bundleFunctions"
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
