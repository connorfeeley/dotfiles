{ lib
, stdenv
, fetchFromGitHub
, cmake
, pkg-config
, curl
, xorg
, avahi
, qtbase
, qttools
, mkDerivation
, openssl
, wrapGAppsHook
, avahiWithLibdnssdCompat ? avahi.override { withLibdnssdCompat = true; }
, fetchpatch
, gtest

  # MacOS / darwin
, darwin
, ghc_filesystem
, ApplicationServices
, Carbon
, Cocoa
, CoreServices
, ScreenSaver

, gitUpdater
}:

mkDerivation rec {
  pname = "input-leap";
  version = "2.5.0-pre";

  src = fetchFromGitHub {
    owner = "input-leap";
    repo = pname;
    rev = "68aac94193a721e4c20512434cf6ab40a2dd89a0";
    sha256 = "sha256-LhpPbpX4MgEkay9Uctjw/UD1J/4nL0LDiLuu6ixTW8k=";
    fetchSubmodules = true;
  };

  buildInputs = [
    curl
    qtbase
    qttools
  ] ++ lib.optionals stdenv.isLinux [
    xorg.libX11
    xorg.libXext
    xorg.libXrandr
    xorg.libXinerama
    xorg.libXtst
    xorg.libICE
    xorg.libSM
    avahiWithLibdnssdCompat
  ] ++ lib.optionals stdenv.isDarwin [
    openssl
    ghc_filesystem
    ApplicationServices
    Carbon
    Cocoa
    CoreServices
    ScreenSaver
  ] ++ lib.optionals (stdenv.isDarwin && darwin.apple_sdk.frameworks ? UserNotifications) [
    darwin.apple_sdk.frameworks.UserNotifications
  ];

  nativeBuildInputs = [
    cmake
    pkg-config
    wrapGAppsHook
    gtest
  ];

  enableParallelBuilding = true;

  # Fix RPATH, and don't build the upstream MacOS bundle target automatically
  preConfigure = lib.optionals stdenv.isDarwin ''
    substituteInPlace CMakeLists.txt \
      --replace 'set (CMAKE_INSTALL_RPATH "@loader_path/../Libraries;@loader_path/../Frameworks")' "" \
      --replace "DEPENDS input-leap input-leaps input-leapc" ""
  '';

  postFixup = lib.optionalString stdenv.isLinux ''
    substituteInPlace "$out/share/applications/input-leap.desktop" --replace "Exec=barrier" "Exec=$out/bin/input-leap"
  '';

  qtWrapperArgs = [
    ''--prefix PATH : ${lib.makeBinPath [ openssl ]}''
  ];

  postInstall = lib.optionals stdenv.isDarwin ''
    mkdir -p $out/Applications
    cp -r bundle/Barrier.app $out/Applications/Barrier.app

    # Link binaries into the bundle so that they are added to system path
    mkdir -p $out/bin
    ln -s $out/Applications/Barrier.app/Contents/MacOS/barrier $out/bin/barrier
    ln -s $out/Applications/Barrier.app/Contents/MacOS/barrierc $out/bin/barrierc
    ln -s $out/Applications/Barrier.app/Contents/MacOS/barriers $out/bin/barriers
  '';

  cmakeFlags = [
    # Don't use vendored gtest
    "-DINPUTLEAP_USE_EXTERNAL_GTEST=ON"

    # The bundling script is patched out, but we still want
    # PkgInfo, Info.plist, and the icon copied to the bundle
    "-DINPUTLEAP_BUILD_INSTALLER=ON"
  ];

  passthru.updateScript = gitUpdater {
    url = "https://github.com/input-leap/input-leap.git";
    rev-prefix = "v";
  };

  meta = {
    description = "Open-source KVM software";
    longDescription = ''
      Input leap is KVM software forked from Symless's synergy 1.9 codebase.
      Synergy was a commercialized reimplementation of the original
      CosmoSynergy written by Chris Schoeneman.
    '';
    homepage = "https://github.com/input-leap/input-leap";
    downloadPage = "https://github.com/input-leap/input-leap/releases";
    license = lib.licenses.gpl2;
    maintainers = [ lib.maintainers.phryneas lib.maintainers.cfeeley ];
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
    mainProgram = "barrier";
  };
}
