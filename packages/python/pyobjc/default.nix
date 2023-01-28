{ lib
, stdenv
, python3
  # , buildPythonPackage
  # , fetchPypi
, libffi

, darwin
, xcbuild
, xcbuildHook

  # Darwin dependencies
, frameworks
}:

python3.pkgs.buildPythonPackage rec {
  pname = "pyobjc-core";
  version = "9.0.1";

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#chap-pkgs-fetchers
  src = python3.pkgs.fetchPypi {
    inherit pname version;
    sha256 = "sha256-XOFRC7C9/1J8WXB5pCsuE6GbdZLnaFC+eWCid1tZySk=";
  };

  # Darwin stdenv unsets SDKROOT, so we can't set it as an attr
  preConfigure = ''
    # Otherwise Nix's libffi headers can't be found
    # substituteInPlace Modules/objc/selector.h \
    #                   Modules/objc/libffi_extra.h \
    #                   Modules/objc/libffi_support.h \
    #                   Modules/objc/libffi_extra.m \
    #                   --replace "#include <ffi/ffi.h>" "#include <ffi.h>"
    # substituteInPlace setup.py \
    #                   --replace "/usr/bin/xcrun" "xcrun"
    for i in $(find . -type f -name "*.py"); do
      substituteInPlace $i \
        --replace '_subprocess.check_output(["sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"' \
        --replace 'subprocess.check_output(["sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"' \
        --replace 'subprocess.check_output(["/usr/bin/sw_vers", "-productVersion"])' 'b"${darwin.apple_sdk.MacOSX-SDK.version}"'
    done
  '';

  # NIX_DEBUG = true;
  enableParallelBuilding = true;
  # makeFlags = [ "SDKROOT=${darwin.apple_sdk.MacOSX-SDK}" ];
  # List of flags passed to `setup.py build_ext` command.
  setupPyBuildFlags = [
    "--no-warnings-as-errors"
    "--inplace"
    "--no-lto"
    "--sdk-root=${darwin.apple_sdk_11_0.MacOSX-SDK}"
  ] ++ lib.optionals (lib.versionAtLeast stdenv.hostPlatform.darwinMinVersion "11") [
    "--deployment-target=11.0"
  ];

  # nativeBuildInputs = lib.optionals stdenv.isDarwin [ xcbuild ];

  NIX_CFLAGS_COMPILE = lib.optionals stdenv.cc.isGNU [
  ] ++ lib.optionals stdenv.isDarwin [
    # "-DMAC_OS_X_VERSION_MAX_ALLOWED=MAC_OS_X_VERSION_10_12" "-DMAC_OS_X_VERSION_MIN_REQUIRED=MAC_OS_X_VERSION_10_12"
    "-DTARGET_OS_OSX"
    "-DTARGET_OS_IPHONE=0"
    "-DTARGET_OS_WATCH=0"

    "-Wno-elaborated-enum-base"

    # Resolve ffi includes and library
    "-I${darwin.apple_sdk.MacOSX-SDK}/usr/include"
    "-L${darwin.apple_sdk.MacOSX-SDK}/usr/lib"

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

    #
    # Prevent errors like
    # /nix/store/xxx-apple-framework-CoreData/Library/Frameworks/CoreData.framework/Headers/NSEntityDescription.h:51:7:
    # error: pointer to non-const type 'id' with no explicit ownership
    #     id** _kvcPropertyAccessors;
    #
    # TODO remove when new Apple SDK is in
    #
    "-fno-objc-arc"
  ];
  # See the guide for more information: https://nixos.org/nixpkgs/manual/#ssec-stdenv-dependencies
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
  buildInputs = [
  ];

  # Output from when this was commented out: building 'objc._objc' extension
  # Therefore I think it's right
  pythonImportsCheck = [ "objc._objc" ];

  # checkInputs = with python3.pkgs; [
  #   # aiodns
  #   # aiohttp
  #   # flask
  #   # mock
  #   pytest
  #   pytest-trio
  #   pytest-asyncio
  #   pytestCheckHook
  #   # trio
  # ];
  # pytestFlagsArray = [ "PyObjCTest/" ];

  # See the guide for more information: https://nixos.org/nixpkgs/manual/#chap-meta
  meta = with lib; {
    description = "Python<->ObjC Interoperability Module";
    homepage = "https://github.com/ronaldoussoren/pyobjc";
    license = licenses.mit;
    maintainers = with maintainers; [ cfeeley ];
  };
}
