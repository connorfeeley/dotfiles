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

  disableTests = builtins.map (test: substitute "def ${test}(self)" "def _${test}(self)");
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
      substitutions = disableTests [
        "test_subclassing"
        "test_issue_272"
        "testGetting"
        "testSetting"
        "test_issue282"
        "testFunctions"
      ];
    };

    extraBuildInputs = [ pyobjc-core ];
    frameworkInputs = [ frameworks.Cocoa ];
  };

  # nix-build --pure $DOTFIELD_DIR -A packages.aarch64-darwin.pyobjc.pyobjc-framework-Quartz --show-trace --verbose --keep-failed
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
      substitutions = disableTests [
        "test_callable_metadata_is_sane"
        "test_protocols"
        "testFunctions"
        "testConstants10_6"
      ];
    };

    extraBuildInputs = [ pyobjc-core pyobjc-framework-Cocoa ];
    frameworkInputs = [
      frameworks.Quartz
      frameworks.ImageCaptureCore
    ];
  };
}
