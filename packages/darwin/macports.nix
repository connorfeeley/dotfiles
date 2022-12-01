{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  version = "";
  name = "-${version}";

  src = fetchurl {
    url = "";
    sha256 = "";
  };

  buildInputs = [  ];

  meta = {
    description = "";
    homepage = https://;
    license = stdenv.lib.licenses.;
    maintainers = [ stdenv.lib.maintainers. ];
    platforms = stdenv.lib.platforms.;
  };
}
