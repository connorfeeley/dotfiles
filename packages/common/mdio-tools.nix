{ lib
, stdenv
, fetchurl
, pkg-config
, libmnl
, ...
}:
let
  # Base package definition
  mdio-tools-base = mainProgram: stdenv.mkDerivation rec {
    pname = "mdio-tools-${mainProgram}";
    version = "1.2.0";
    src = fetchurl {
      url = "https://github.com/wkz/mdio-tools/releases/download/${version}/mdio-tools-${version}.tar.gz";
      sha256 = "01m1y8zzjlaq91sayxx314am462rdw5gp90vvb0zd4i3qqqp9qf5";
    };
    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ libmnl ];

    meta = with lib; {
      inherit mainProgram;
      description = "Low-level debug tools for MDIO devices.";
      homepage = "https://github.com/wkz/mdio-tools";
      license = licenses.gpl2;
      platforms = platforms.linux;
      maintainers = [ maintainers.cfeeley ];
    };
  };

  # Two derivations, so that separate AppImages can be built.
  # AppImage for 'mvls' program can be built with:
  # NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nix bundle --impure --print-build-logs --bundler \
  #     github:ralismark/nix-appimage#bundlers.aarch64-linux.default \
  #     .#packages.aarch64-linux.mdio-tools.passthru.mvls
  # AppImage for 'mdio' program can be built with:
  # NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nix bundle --impure --print-build-logs --bundler \
  #     github:ralismark/nix-appimage#bundlers.aarch64-linux.default \
  #     .#packages.aarch64-linux.mdio-tools
  mdio-tools-mvls = mdio-tools-base "mvls";
  mdio-tools-mdio = mdio-tools-base "mdio" // { passthru.mvls = mdio-tools-mvls; };
in
mdio-tools-mdio
