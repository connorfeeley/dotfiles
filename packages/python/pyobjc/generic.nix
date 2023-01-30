{ lib, stdenv
, python3Packages # WARNING: may be broken on python38! "offsetof(....., vectorcall)" or something
, fetchFromGitHub, libffi, darwin, frameworks, xcbuild, xcbuildHook
, pname, pythonImportsCheck, pytestFlagsArray, disabledTestPaths, disabledTests, doCheck ? false
}:
python3Packages.buildPythonApplication rec {
  inherit pname;
  version = "9.0.1";

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#chap-pkgs-fetchers
  src = fetchFromGitHub {
    owner = "ronaldoussoren";
    repo = "pyobjc";
    rev = "v${version}";
    sha256 = "sha256-R5Eul7W6bubYOtajod6gQvqkS+YYlPyy5LC3YT5AbUg=";
  };

  sourceRoot = "source/${pname}";

  PIP_DISABLE_PIP_VERSION_CHECK = 1;

  preConfigure = ''
    for i in $(find . -type f -name "*.py"); do
      substituteInPlace $i \
        --replace '_subprocess.check_output(["sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"' \
        --replace 'subprocess.check_output(["sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"' \
        --replace 'subprocess.check_output(["/usr/bin/sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"'
    done
  '';

  enableParallelBuilding = true;

  # List of flags passed to `setup.py build_ext` command.
  setupPyBuildFlags = [
    "--no-warnings-as-errors"
    # "--inplace"
    "--no-lto"
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
  ];
  buildInputs = [ darwin.libobjc xcbuild ];

  # TODO: run c tests ('make c-coverage')
  inherit pythonImportsCheck pytestFlagsArray disabledTestPaths disabledTests doCheck;

  meta = with lib; {
    description = "Python<->ObjC Interoperability Module";
    homepage = "https://github.com/ronaldoussoren/pyobjc";
    license = licenses.mit;
    maintainers = with maintainers; [ cfeeley ];
  };
}
