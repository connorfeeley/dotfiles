{ lib
, stdenv
, fetchurl
, buildFHSUserEnv
, writeScript
}:
let
  version = "${releaseMajor}-${releaseMinor}";
  releaseMajor = "2021";
  releaseMinor = "2";
  url = "http://petalinux.xilinx.com/sswreleases/rel-v${releaseMajor}/xsct-trim/xsct-${releaseMajor}-${releaseMinor}.tar.xz";

  xsct = stdenv.mkDerivation {
    pname = "xsct";
    inherit version;

    src = fetchurl {
      inherit url;
      sha256 = "b038e9f101c68ae691616d0976651e2be9d045e1a36d997bfe431c1526ab7a9c";
    };

    installPhase = ''
      set -x
      ls -al
      mkdir -p $out
      cp -r ${releaseMajor}.${releaseMinor}/* $out
    '';

    nativeBuildInputs = [ ];

    buildInputs = [ ];

    meta = with lib; {
      homepage = "https://www.xilinx.com/htmldocs/xilinx2019_1/SDK_Doc/xsct/intro/xsct_introduction.html";
      description = "Xilinx Software Command-Line Tool";
      license = licenses.unfree;
      maintainers = [ maintainers.cfeeley ];
      platforms = platforms.linux;
    };
  };
in
buildFHSUserEnv rec {
  name = "xsct"; # wrapped

  targetPkgs = pkgs: with pkgs; [
    # quartus requirements
    glib
    xorg.libICE
    xorg.libSM
    zlib
    # qsys requirements
    xorg.libXtst
    xorg.libXi
  ];
  multiPkgs = pkgs: with pkgs; let
    # This seems ugly - can we override `libpng = libpng12` for all `pkgs`?
    # freetype = pkgs.freetype.override { libpng = libpng12; };
    # fontconfig = pkgs.fontconfig.override { inherit freetype; };
    # libXft = pkgs.xorg.libXft.override { inherit freetype fontconfig; };
  in
  [
    # modelsim requirements
    libxml2
    ncurses5
    unixODBC
    # libXft
    # common requirements
    freetype
    fontconfig
    libpng12
    protobuf
    xorg.libX11
    xorg.libXext
    xorg.libXrender
    xorg.libXau
    libudev0-shim
    libxcrypt
  ];

  passthru = { inherit xsct; };

  extraInstallCommands = ''
    mkdir -p $out/share/applications $out/share/icons/128x128
    ln -s ${xsct}/licenses/images/dc_quartus_panel_logo.png $out/share/icons/128x128/quartus.png

    mkdir -p $out/quartus/bin $out/quartus/sopc_builder/bin $out/modelsim_ase/bin $out/qprogrammer/bin
    WRAPPER=$out/bin/${name}

    echo "#!${stdenv.shell}" >> $out/$executable
    echo "$WRAPPER ${xsct}/$executable \"\$@\"" >> $out/$executable

  '';

  # LD_PRELOAD fixes issues in the licensing system that cause memory corruption and crashes when
  # starting most operations in many containerized environments, including WSL2, Docker, and LXC
  # (a similiar fix involving LD_PRELOADing tcmalloc did not solve the issue in my situation)
  # we use the name so that quartus can load the 64 bit verson and modelsim can load the 32 bit version
  # https://community.intel.com/t5/Intel-FPGA-Software-Installation/Running-Quartus-Prime-Standard-on-WSL-crashes-in-libudev-so/m-p/1189032
  runScript = writeScript "${name}-wrapper" ''
    exec env LD_PRELOAD=libudev.so.0 "$@"
  '';
}
