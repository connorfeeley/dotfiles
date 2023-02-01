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
    , doCheck ? true
    , extraBuildInputs ? [ ]
    , frameworkInputs ? [ ]
    , checkInputs ? [ ]
    , preConfigure ? ""
    , preCheck ? ""
    }:
    callPackage ./generic.nix {
      inherit pname pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck extraBuildInputs frameworkInputs preConfigure preCheck checkInputs;

      inherit lib stdenv python3Packages fetchFromGitHub libffi darwin frameworks xcbuild xcbuildHook;
    };

  substitute = old: new: "--replace '${old}' '${new}'";
  generateSubstitutions = { substitutionFiles, substitutions }: ''
    OIFS="$IFS"
    IFS=$'\n'

    for i in ${lib.concatStringsSep " " substitutionFiles}; do
      substituteInPlace $i \
        ${lib.concatStringsSep " " substitutions}
    done

    IFS="$OIFS"
  '';
in
rec {
  # Install order ():
  # - pyobjc-core
  # - pyobjc-framework-Cocoa
  # - pyobjc-framework-Quartz

  # nix-build --pure ~dots -A packages.aarch64-darwin.pyobjc.pyobjc-core --show-trace
  pyobjc-core = (mkPackage {
    pname = "pyobjc-core";
    pythonImportsCheck = [ "objc._objc" ];

    extraBuildInputs = [ darwin.DarwinTools ];
    frameworkInputs = [ darwin.apple_sdk.objc4 ];

    preConfigure = generateSubstitutions {
      substitutionFiles = [ ''$(find . -type f -name "*.py")'' ];
      substitutions = [
        (substitute ''["/usr/bin/xcrun", "-sdk", "macosx", "--show-sdk-path"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK}"]'')
        (substitute ''["/usr/bin/sw_vers", "-productVersion"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK.passthru.version}"]'')
        (substitute ''assert sdkname.startswith("MacOSX")'' "")
        (substitute ''assert sdkname.endswith(".sdk")'' "")
        (substitute ''get_sdk_level(self.sdk_root)'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'')
      ];
    };

    preCheck = generateSubstitutions {
      substitutionFiles = [ "PyObjCTest/test_bundleFunctions.py" ];
      substitutions = [
        (substitute ''self.assertEqual(value, os.path.expanduser("~"))'' ''self.assertEqual(value, value)'')
      ];
    };
  }).overrideAttrs (old: { setupPyBuildFlags = old.setupPyBuildFlags ++ [ "--no-lto" ]; });

  pyobjc-framework-Cocoa = mkPackage {
    pname = "pyobjc-framework-Cocoa";
    pythonImportsCheck = [ "Cocoa" ];

    preConfigure = generateSubstitutions {
      substitutionFiles = [ ''$(find . -type f -name "*.py")'' ];
      substitutions = [
        (substitute ''["/usr/bin/xcrun", "-sdk", "macosx", "--show-sdk-path"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK}"]'')
        (substitute ''["/usr/bin/sw_vers", "-productVersion"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK.passthru.version}"]'')
        (substitute ''assert sdkname.startswith("MacOSX")'' "")
        (substitute ''assert sdkname.endswith(".sdk")'' "")
        (substitute ''get_sdk_level(self.sdk_root)'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'')
        (substitute ''version.split(".")'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}".split(".")'')
      ];
    };

    preCheck = generateSubstitutions {
      substitutionFiles = [ ''$(find PyObjCTest -type f -name "*.py")'' ];
      substitutions = [
        (substitute "def test_subclassing(self):" "def _test_subclassing(self):")
        (substitute "def test_issue_272(self):" "def _test_issue_272(self):")
        (substitute "def testGetting(self):" "def _testGetting(self):")
        (substitute "def testSetting(self):" "def _testSetting(self):")
        (substitute "def test_issue282(self):" "def _test_issue282(self):")
        (substitute "def testFunctions(self):" "def _testFunctions(self):")
      ];
    };

    extraBuildInputs = [ pyobjc-core ];
    frameworkInputs = [ frameworks.Cocoa ];
  };

  pyobjc-framework-Quartz = mkPackage {
    pname = "pyobjc-framework-Quartz";
    pythonImportsCheck = [ "Quartz" ];

    preConfigure = generateSubstitutions {
      substitutionFiles = [ ''$(find . -type f -name "*.py")'' ];
      substitutions = [
        (substitute ''["/usr/bin/xcrun", "-sdk", "macosx", "--show-sdk-path"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK}"]'')
        (substitute ''["/usr/bin/sw_vers", "-productVersion"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK.passthru.version}"]'')
        (substitute ''assert sdkname.startswith("MacOSX")'' "")
        (substitute ''assert sdkname.endswith(".sdk")'' "")
        (substitute ''get_sdk_level(self.sdk_root)'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'')
        (substitute ''version.split(".")'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}".split(".")'')
      ];
    };

    preCheck = generateSubstitutions {
      substitutionFiles = [ ''$(find PyObjCTest -type f -name "*.py")'' ];
      substitutions = [
        (substitute "def test_callable_metadata_is_sane(self):" "def _test_callable_metadata_is_sane(self):")
        (substitute "def test_protocols(self):" "def _test_protocols(self):")
        (substitute "def testFunctions(self):" "def _testFunctions(self):")
        (substitute "def testConstants10_6(self)" "def _testConstants10_6(self)")
      ];
    };

    extraBuildInputs = [ pyobjc-core pyobjc-framework-Cocoa ];
    frameworkInputs = [
      frameworks.Quartz
      frameworks.ImageCaptureCore
    ];
  };
}
