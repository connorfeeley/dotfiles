{ lib
, stdenv
, fetchurl
}:
let
  version = "${releaseMajor}-${releaseMinor}";
  releaseMajor = "2021";
  releaseMinor = "2";
  url = "http://petalinux.xilinx.com/sswreleases/rel-v${releaseMajor}/xsct-trim/xsct-${releaseMajor}-${releaseMinor}.tar.xz";
in
stdenv.mkDerivation {
  pname = "xsct";
  inherit version;

  src = fetchurl {
    inherit url;
    sha256 = "b038e9f101c68ae691616d0976651e2be9d045e1a36d997bfe431c1526ab7a9c";
  };

  nativeBuildInputs = [ ];

  buildInputs = [ ];

  meta = with lib; {
    homepage = "https://www.xilinx.com/htmldocs/xilinx2019_1/SDK_Doc/xsct/intro/xsct_introduction.html";
    description = "Xilinx Software Command-Line Tool";
    license = licenses.proprietary;
    maintainers = [ maintainers.cfeeley ];
    platforms = [ "x86_64-linux" ];
  };
}
