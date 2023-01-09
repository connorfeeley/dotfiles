{ lib
, stdenv
, fetchFromGitHub
, cmake
, pkg-config
, curl
, xorg
, avahi
, qtbase
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
  version = "2.4.0";

  src = fetchFromGitHub {
    owner = "input-leap";
    repo = pname;
    rev = "165a5c00a175128ee0afd95c15dd4a0e06d38f16";
    sha256 = "sha256-eLNeWkNEKkxFBe7FSTfF0uQC5aTemfD2MQwWXYed3wI=";
    fetchSubmodules = true;
  };

  patches = lib.optionals stdenv.isDarwin [
    ./0001-darwin-ssl-libs.patch
    ./0002-darwin-bundle.patch
  ];

  buildInputs = [
    curl
    qtbase
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

  postFixup = lib.optionalString stdenv.isLinux ''
    substituteInPlace "$out/share/applications/barrier.desktop" --replace "Exec=barrier" "Exec=$out/bin/barrier"
  '';

  qtWrapperArgs = [
    ''--prefix PATH : ${lib.makeBinPath [ openssl ]}''
  ];

  postInstall = lib.optionals stdenv.isDarwin ''
    mkdir -p $out/Applications
    cp -r bundle/Barrier.app $out/Applications/Barrier.app
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
