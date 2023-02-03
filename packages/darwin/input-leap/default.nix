# Opening nixpkgs PR shortly - canonical source location is:
# https://git.sr.ht/~cfeeley/nixpkgs/tree/feat/input-leap/item/pkgs/applications/misc/input-leap/default.nix
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
, openssl
, wrapQtAppsHook
, avahiWithLibdnssdCompat ? avahi.override { withLibdnssdCompat = true; }
, gtest
, gitUpdater

  # MacOS / darwin
, ghc_filesystem
, ApplicationServices
, Carbon
, Cocoa
, CoreServices
, ScreenSaver
, UserNotifications
}:

stdenv.mkDerivation rec {
  pname = "input-leap";
  version = "2.5.0-pre";

  src = fetchFromGitHub {
    owner = "input-leap";
    repo = pname;
    rev = "68aac94193a721e4c20512434cf6ab40a2dd89a0";
    sha256 = "sha256-LhpPbpX4MgEkay9Uctjw/UD1J/4nL0LDiLuu6ixTW8k=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake pkg-config wrapQtAppsHook gtest qttools ];

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
    ghc_filesystem
    ApplicationServices
    Carbon
    Cocoa
    CoreServices
    ScreenSaver
    UserNotifications
  ];

  patches = lib.optionals stdenv.isDarwin [ ./0001-darwin-ssl-libs.patch ];

  enableParallelBuilding = true;

  cmakeFlags = [
    # Don't use vendored gtest
    "-DINPUTLEAP_USE_EXTERNAL_GTEST=ON"

    # Build the tests (requires gtest)
    "-DINPUTLEAP_BUILD_TESTS=ON"

    # The bundling script is patched out, but we still want
    # PkgInfo, Info.plist, and the icon copied to the bundle
    "-DINPUTLEAP_BUILD_INSTALLER=ON"
  ];

  # Fix RPATH, and don't build the upstream MacOS bundle target automatically
  preConfigure = lib.optionals stdenv.isDarwin ''
    substituteInPlace CMakeLists.txt \
      --replace 'set (CMAKE_INSTALL_RPATH "@loader_path/../Libraries;@loader_path/../Frameworks")' "" \
      --replace "DEPENDS input-leap input-leaps input-leapc" "" \
      --replace "add_custom_target(InputLeap_MacOS ALL" "add_custom_target(InputLeap_MacOS" \
      ${lib.optionalString stdenv.isDarwin
        # Input leap assumes an older version of QT, which has deprecated pixmap-related functions
        ''
        --replace "-DGTEST_USE_OWN_TR1_TUPLE=1" "-DGTEST_USE_OWN_TR1_TUPLE=1 -Wno-deprecated-declarations"
      ''}
  '';

  postFixup = lib.optionalString stdenv.isLinux ''
    substituteInPlace "$out/share/applications/input-leap.desktop" --replace "Exec=barrier" "Exec=$out/bin/input-leap"
  '';

  qtWrapperArgs = [
    ''--prefix PATH : ${lib.makeBinPath [ openssl ]}''
  ];

  postInstall = lib.optionals stdenv.isDarwin ''
    mkdir -p $out/Applications
    cp -r bundle/InputLeap.app $out/Applications/InputLeap.app

    # Link binaries into the bundle so that they are added to system path
    mkdir -p $out/bin
    ln -s $out/Applications/InputLeap.app/Contents/MacOS/input-leap  $out/bin/input-leap
    ln -s $out/Applications/InputLeap.app/Contents/MacOS/input-leapc $out/bin/input-leapc
    ln -s $out/Applications/InputLeap.app/Contents/MacOS/input-leaps $out/bin/input-leaps
  '';

  doCheck = stdenv.isLinux;

  passthru.updateScript = gitUpdater { rev-prefix = "v"; };

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
    mainProgram = "input-leap";
  };
}
