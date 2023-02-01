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
    , preConfigure ? preConfigureDefault
    }:
    callPackage ./generic.nix {
      inherit pname pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck extraBuildInputs frameworkInputs checkInputs;

      inherit lib stdenv python3Packages fetchFromGitHub libffi darwin frameworks xcbuild xcbuildHook;

      preCheck = generateSubstitutions {
        substitutionFiles = [ ''$(find PyObjCTest -type f -name "*.py")'' ];
        substitutions = disableTests disabledTests;
      };
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

  # Base preConfigure: applies to all packages
  preConfigureCore = generateSubstitutions {
    substitutionFiles = [ ''$(find . -type f -name "*.py")'' ];
    substitutions = [
      (substitute ''["/usr/bin/xcrun", "-sdk", "macosx", "--show-sdk-path"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK}"]'')
      (substitute ''["/usr/bin/sw_vers", "-productVersion"]'' ''["echo", "${darwin.apple_sdk.MacOSX-SDK.passthru.version}"]'')
      (substitute ''assert sdkname.startswith("MacOSX")'' "")
      (substitute ''assert sdkname.endswith(".sdk")'' "")
      (substitute ''get_sdk_level(self.sdk_root)'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'')
    ];
  };
  # Default preConfigure: applies to all packages except pyobjc-core
  preConfigureDefault = generateSubstitutions {
    inherit (preConfigureCore) substitutionFiles;
    substitutions = preConfigureCore.substitutions ++ [
      (substitute ''version.split(".")'' ''"${darwin.apple_sdk.MacOSX-SDK.passthru.version}".split(".")'')
    ];
  };

  # Remaining framework wrappers - can be installed in any order (after pyobjc-core, pyobjc-framework-Cocoa, and pyobjc-framework-Quartz)
  frameworkWrappers =
    let all = (import ./framework-wrappers.nix).FRAMEWORK_WRAPPERS;
    in
    lib.filter (e: e.name != "Cocoa" && e.name != "Quartz") all;
in
rec {
  # Install order ():
  # - pyobjc-core
  # - pyobjc-framework-Cocoa
  # - pyobjc-framework-Quartz

  ###
  ### Core package
  ###   nix-build --pure ~dots -A packages.aarch64-darwin.pyobjc.pyobjc-core
  pyobjc-core = (mkPackage {
    pname = "pyobjc-core";
    pythonImportsCheck = [ "objc._objc" ];

    preConfigure = preConfigureCore;
    extraBuildInputs = [ darwin.DarwinTools ];
    frameworkInputs = [ darwin.apple_sdk.objc4 ];
  }).overrideAttrs (old: {
    setupPyBuildFlags = old.setupPyBuildFlags ++ [ "--no-lto" ];
    preCheck = old.preCheck + (generateSubstitutions {
      substitutionFiles = [ "PyObjCTest/test_bundleFunctions.py" "PyObjCTest/test_bundleFunctions.py" ];
      substitutions = disableTests [ "testSimple" ];
    });
  });

  ###
  ### Cocoa framework
  ###   nix-build --pure ~dots -A packages.aarch64-darwin.pyobjc.pyobjc-framework-Cocoa
  pyobjc-framework-Cocoa = mkPackage {
    pname = "pyobjc-framework-Cocoa";
    pythonImportsCheck = [ "Cocoa" ];

    disabledTests = [
      "test_subclassing"
      "test_issue_272"
      "testGetting"
      "testSetting"
      "test_issue282"
      "testFunctions"
    ];

    extraBuildInputs = [ pyobjc-core ];
    frameworkInputs = [ frameworks.Cocoa ];
  };

  ###
  ### Quartz framework
  ###   nix-build --pure ~dots -A packages.aarch64-darwin.pyobjc.pyobjc-framework-Quartz
  pyobjc-framework-Quartz = mkPackage {
    pname = "pyobjc-framework-Quartz";
    pythonImportsCheck = [ "Quartz" ];

    disabledTests = [
      "test_callable_metadata_is_sane"
      "test_protocols"
      "testFunctions"
      "testConstants10_6"
    ];

    extraBuildInputs = [ pyobjc-core pyobjc-framework-Cocoa ];
    frameworkInputs = [
      frameworks.Quartz
      frameworks.ImageCaptureCore
    ];
  };
}
