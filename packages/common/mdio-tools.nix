{ lib
, stdenv
, fetchurl
, pkg-config
, libmnl
, ...
}:
stdenv.mkDerivation rec {
  name = "mdio-tools";
  version = "1.2.0";
  src = fetchurl {
    url = "https://github.com/wkz/mdio-tools/releases/download/${version}/mdio-tools-${version}.tar.gz";
    sha256 = "01m1y8zzjlaq91sayxx314am462rdw5gp90vvb0zd4i3qqqp9qf5";
  };
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libmnl ];

  # makeFlags = [ "BINDIR=$(out)/bin" "MANPATH=$(out)/share/man" ];
  # installTargets = [ "install" "install.man" ];

  meta = with lib; {
    description = "Low-level debug tools for MDIO devices.";
    homepage = "https://github.com/wkz/mdio-tools";
    license = licenses.gpl2;
    maintainers = [ maintainers.cfeeley ];
  };
}
