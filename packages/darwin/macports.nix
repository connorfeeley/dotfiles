{ lib, stdenv, fetchurl, autoreconfHook, curl }:

stdenv.mkDerivation rec {
  version = "2.8.0";
  name = "macports-base-v${version}";

  src = fetchurl {
    url = "https://github.com/macports/macports-base/releases/download/v${version}/MacPorts-${version}.tar.gz";
    sha256 = "0yh036bjpmnscdgs4g9yc37hzayfxdjzy80a30qvd7ahhjvr2rqh";
  };

  buildInputs = [ curl ];

  # Passes invalid "-arch <arch>" flag.
  # Hoped autoreconfHook would fix that.
  nativeBuildInputs = [ autoreconfHook ];

  # TODO: support x86_64-darwin as well
  configureFlags = [
    ''--with-universal-archs="arm64"''
    "--with-no-root-privileges"
  ];

  patches = [
    ./0001-fix-don-t-use-BSD-chmod-syntax.patch
  ];

  meta = {
    description = "";
    homepage = "https://macports.org";
    license = lib.licenses.bsd3;
    maintainers = [ lib.maintainers.cfeeley ];
    platforms = lib.platforms.darwin;
  };
}
