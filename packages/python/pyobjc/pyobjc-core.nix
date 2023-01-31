# https://raw.githubusercontent.com/NixOS/nixpkgs/852c2c65bbc34a4afeaa20f64072db9909bef572/pkgs/development/python-modules/pyobjc-core/default.nix
{ lib, stdenv, buildPythonPackage, pythonOlder, fetchPypi, darwin, python, cffi, setuptools}:

buildPythonPackage rec {
  pname = "pyobjc-core";
  version = "6.2.2";

  disabled = pythonOlder "3.6";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0z42b8nrgfa2wfxds5pwc0j8iysjkqi7i2xj33fxlf940idb3rrq";
  };

  postPatch = ''
    # Hard code correct SDK version
    # Fix hardcoded paths
    # Remove xcrun call, all paths are provided by nix anyway
    substituteInPlace setup.py \
      --replace 'get_sdk_level(self.sdk_root)' '"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"' \
      --replace 'os.path.join(self.sdk_root, "usr/include/objc/runtime.h")' '"${darwin.objc4}/include/objc/runtime.h"' \
      --replace '["/usr/bin/xcrun", "-sdk", "macosx", "--show-sdk-path"]' '["true"]'

    # Hard code OS version
    # This needs to be done here or pyobjc-frameworks-* don't get the change
    substituteInPlace Lib/PyObjCTools/TestSupport.py \
      --replace 'return ".".join(v.split("."))' 'return "${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'
  '';

  buildInputs = [
    cffi
  ] ++ (with darwin.apple_sdk.frameworks; [
    Carbon
    Cocoa
    Foundation
  ]);

  propagatedBuildInputs = [
    # required for namespaced import of PyObjCTools.TestSupport from pyobjc-framework-*
    setuptools
  ];

  hardeningDisable = [ "strictoverflow" ];

  preBuild = ''
    export SDKROOT=/
  '';

  preCheck = ''
    # Test removed because ~ does not expand to /homeless-shelter
    rm PyObjCTest/test_bundleFunctions.py

    # xcrun is not available and nix paths do not match the hardcoded paths
    # https://github.com/ronaldoussoren/pyobjc/blob/efecb479c3cf32ec1d36abfb9b7d7c8cfd4ede6d/pyobjc-core/PyObjCTest/test_dyld.py#L207
    substituteInPlace PyObjCTest/test_dyld.py \
      --replace 'p = subprocess.check_output(["xcrun", "--show-sdk-path"]).strip()' 'return True'

    # Tests in PyObjCTest/test_bridgesupport.py return "unexpected success" for CoreAudio.bridgesupport and MetalPerformanceShaders
    # removing them from BROKEN_FRAMEWORKS does no work and leads to:
    #   objc.internal_error: Invalid array definition in type signature: [
    rm PyObjCTest/test_bridgesupport.py

    substituteInPlace PyObjCTest/test_testsupport.py \
      --replace '".".join(platform.mac_ver()[0].split("."))' '"${darwin.apple_sdk.MacOSX-SDK.passthru.version}"'

    # Tries to acces paths outside the sandbox
    rm PyObjCTest/test_filepointer.py PyObjCTest/test_fsref.py

    # Segmentation fault: 11
    rm PyObjCTest/test_corefoundation.py
  '';

  # show test names instead of just dots
  checkPhase = ''
    runHook preCheck

    ${python.interpreter} setup.py test --verbosity=3

    runHook postCheck
  '';

  meta = with lib; {
    description = "Python<->ObjC Interoperability Module";
    homepage = "https://pythonhosted.org/pyobjc-core/";
    license = licenses.mit;
    maintainers = with maintainers; [ SuperSandro2000 ];
    platforms = platforms.darwin;
  };
}
