{ lib
, stdenv
, python3Packages # WARNING: may be broken on python38! "offsetof(....., vectorcall)" or something
, fetchFromGitHub
, libffi
, darwin
, frameworks
, xcbuild
, xcbuildHook
, pname
, pythonImportsCheck
, pytestFlagsArray
, disabledTestPaths
, disabledTests
, doCheck ? false
, extraBuildInputs ? [ ]
, frameworkInputs ? [ ]
, checkInputs ? [ ]
, preCheck ? ""
}:
python3Packages.buildPythonApplication rec {
  inherit pname;
  version = "9.1";

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#chap-pkgs-fetchers
  src = fetchFromGitHub {
    owner = "ronaldoussoren";
    repo = "pyobjc";
    rev = "v9.0.1";
    sha256 = "sha256-R5Eul7W6bubYOtajod6gQvqkS+YYlPyy5LC3YT5AbUg=";
  };

  sourceRoot = "source/${pname}";

  PIP_DISABLE_PIP_VERSION_CHECK = 1;

  preConfigure = ''
    OIFS="$IFS"
    IFS=$'\n'

    for i in $(find . -type f -name "*.py"); do
      substituteInPlace $i \
        --replace '["/usr/bin/xcrun", "-sdk", "macosx", "--show-sdk-path"]' '["echo", "${darwin.apple_sdk.MacOSX-SDK}"]' \
        --replace '["/usr/bin/sw_vers", "-productVersion"]' '["echo", "${darwin.apple_sdk.MacOSX-SDK.passthru.version}"]' \
        --replace 'assert sdkname.startswith("MacOSX")' "" \
        --replace 'assert sdkname.endswith(".sdk")' "" \
        --replace 'get_sdk_level(self.sdk_root)' '"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'


        # --replace '_subprocess.check_output(["sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"' \
        # --replace 'subprocess.check_output(["sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"' \
        # --replace 'subprocess.check_output(["/usr/bin/sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"'
    done

    IFS="$OIFS"
  '';

  enableParallelBuilding = true;

  # List of flags passed to `setup.py build_ext` command.
  setupPyBuildFlags = [
    "--no-warnings-as-errors"
    "--inplace"
    "--no-lto"
    # "--sdk-root=${darwin.apple_sdk.MacOSX-SDK}"
  ];

  NIX_CFLAGS_COMPILE = lib.optionals stdenv.cc.isGNU [ ]
    ++ lib.optionals stdenv.isDarwin [
    "-DTARGET_OS_OSX"
    "-DTARGET_OS_IPHONE=0"
    "-DTARGET_OS_WATCH=0"

    # Resolve ffi includes and library
    "-I${darwin.apple_sdk.MacOSX-SDK}/usr/include"
    "-L${darwin.apple_sdk.MacOSX-SDK}/usr/lib"

    "-Wno-elaborated-enum-base"

    # Suppress warning about nullability:
    #     note: insert '_Nonnull' if the array parameter should never be null
    "-Wno-nullability-completeness"

    # Suppress 'warning: unknown platform 'macCatalyst' in availability macro'
    "-Wno-availability"

    # 'warning: macro expansion producing 'defined' has undefined behavior'
    "-Wno-expansion-to-defined"

    # 'warning: unused parameter'
    "-Wno-unused-parameter"

    # warning: argument unused during compilation: '-fno-strict-overflow'
    "-Wno-unused-command-line-argument"

    # 'warning: unused function'
    "-Wno-unused-function"

    # warning: cast of type 'SEL' to 'const char *' is deprecated; use sel_getName instead
    "-Wno-cast-of-sel-type"
  ];

  propagatedBuildInputs = with frameworks; [
    # libffi
    # Foundation
    CoreFoundation
    CoreServices
    AVFoundation
    Cocoa
    GameplayKit
    SpriteKit
    SceneKit
    GLKit
    MetalPerformanceShaders
    ModelIO
    # required for namespaced import of PyObjCTools.TestSupport from pyobjc-framework-*
    python3Packages.setuptools
  ] ++ frameworkInputs;
  buildInputs = [ darwin.libobjc xcbuild ] ++ extraBuildInputs;
  nativeBuildInputs = [ darwin.DarwinTools xcbuild ];

  # TODO: run c tests ('make c-coverage')
  inherit pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck preCheck checkInputs;

  meta = with lib; {
    description = "Python<->ObjC Interoperability Module";
    homepage = "https://github.com/ronaldoussoren/pyobjc";
    license = licenses.mit;
    maintainers = with maintainers; [ cfeeley ];
  };
}
