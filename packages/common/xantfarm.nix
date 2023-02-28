{ stdenv, fetchurl, imake, gccmakedep, xorg, lib, ... }:
stdenv.mkDerivation {
  name = "xantfarm";
  version = "1.0.0";
  src = fetchurl {
    url = "https://acme.com/software/xantfarm/xantfarm_14Aug2014.tar.gz";
    sha256 = "1ci3vs2vi5pdwqxa37ahzvn0x73sbm0m6ms9dkdkr7x9djb230sh";
  };
  nativeBuildInputs = [ imake gccmakedep ];
  buildInputs = with xorg; [ libX11 libXext libXmu ];

  makeFlags = [ "BINDIR=$(out)/bin" "MANPATH=$(out)/share/man" ];
  installTargets = [ "install" "install.man" ];

  meta = with lib; {
    description =
      "Simulates an ant hill and displays it in the root X11 window.";
    homepage = "https://acme.com/software/xantfarm";
    license = licenses.bsd2;
    maintainers = [ maintainers.cfeeley ];
  };
}
