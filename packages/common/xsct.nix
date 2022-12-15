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

    doUnpack = false;

    installPhase = ''
      mkdir -p $out
      tar -xf $src -C $out
      cp -r ${releaseMajor}.${releaseMinor}/* $out
    '';

    postInstall = ''
      substituteInPlace $out/bin/ldlibpath.sh \
          --replace "lsb_release $1 2>/dev/null | sed 's/^.*:[ 	]*//' | tr '[:upper:]' '[:lower:]'" "echo nixos-suse-nixos"
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
  name = "xsct-wrapper"; # wrapped

  targetPkgs = pkgs: with pkgs; [
    openssl
    zlib

    tcl
    tclx
    tcllib

    xorg.xlsclients
    xorg.libXft
    xorg.libXtst
    xorg.libX11
    xorg.libXScrnSaver
    xorg.libXcursor
    xorg.libXext
    xorg.libXft
    xorg.libXi
    xorg.libXrender
    xorg.libXt
    xorg.libXv
    xorg.pixman
    xorg.xorgproto
    xvfb-run
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

  extraInstallCommands =
    let
      executables = [
        "a9-linaro-pre-build-step"
        "aie_import"
        "archive_summary"
        "build-bsp"
        "build_xd_ip_db"
        "build_xd_ip_db.tcl"
        "buildinfo"
        "buildplatform.sh"
        "caller_rewrite"
        "cdoutil"
        "cf2bd"
        "cf2sw"
        "cf2xd"
        "cf_xsd"
        "cf_xsd.tcl"
        "cfgen"
        "clang_wrapper"
        "create_sc_xo"
        "cs_server"
        "data2mem"
        "debug_hw"
        "diffbd"
        "emconfigutil"
        "fa_pack"
        "flash_strip_bw"
        "generate-platform.sh"
        "hls_axilite_map.awk"
        "hls_wrapper_gen"
        "hw_server"
        "hw_serverpv"
        "ipmetadata_config_checker"
        "kernelinfo"
        "launch_emulator"
        "launch_emulator.py"
        "ldlibpath.sh"
        "llvm-link"
        "loader"
        "manage_ipcache"
        "messagepp"
        "mfsgen"
        "mkfatimg"
        "perf_analyze"
        "platforminfo"
        "pragma_gen"
        "pre-build"
        "prep_target"
        "program_flash"
        "program_ftdi"
        "rdiArgs.sh"
        "scripts"
        "sdcard_gen"
        "sdcard_gen.tcl"
        "setEnvAndRunCmd.sh"
        "setupEnv.sh"
        "stub_gen"
        "svf_utility"
        "symbol_server"
        "system_link"
        "tcfgdbclient"
        "tcflog"
        "unsetldlibpath.sh"
        "update-platform.sh"
        "updatemem"
        "v++"
        "vitis"
        "vitis_analyzer"
        "vitisng"
        "vitisng-server"
        "vivado_lab"
        "vlm"
        "vp_analyze"
        "vperf"
        "vpl"
        "wbtcv"
        "xcd"
        "xclbinutil"
        "xgdb"
        "XidanePass"
        "xlicdiag"
        "xrcserver"
        "xrflink"
        "xrt_server"
        "xsct"
        "xsdb"
        "xtclsh"
        "xvc_pcie"
      ];
      executablesStr = lib.concatStringsSep " " executables;
    in
    ''
      mkdir -p $out/share/applications $out/share/icons/128x128
      ln -s ${xsct}/licenses/images/dc_quartus_panel_logo.png $out/share/icons/128x128/quartus.png

      mkdir -p $out/quartus/bin $out/quartus/sopc_builder/bin $out/modelsim_ase/bin $out/qprogrammer/bin
      WRAPPER=$out/bin/${name}
      EXECUTABLES="${lib.concatStringsSep " " executables}"
      for executable in $EXECUTABLES;
      do
        echo "#!${stdenv.shell}" >> $out/$executable
        echo "$WRAPPER ${xsct}/bin/$executable \"\$@\"" >> $out/$executable
      done

      cd $out
      ${lib.optionalString (executablesStr != "") "chmod +x ${executablesStr}"}
      # link into $out/bin so executables become available on $PATH
      ${lib.optionalString (executablesStr != "") "ln --symbolic --relative --target-directory ./bin ${executablesStr}"}
      ln --symbolic --relative ${xsct} wrapped
    '';

  # LD_PRELOAD fixes issues in the licensing system that cause memory corruption and crashes when
  # starting most operations in many containerized environments, including WSL2, Docker, and LXC
  # (a similiar fix involving LD_PRELOADing tcmalloc did not solve the issue in my situation)
  # we use the name so that quartus can load the 64 bit verson and modelsim can load the 32 bit version
  # https://community.intel.com/t5/Intel-FPGA-Software-Installation/Running-Quartus-Prime-Standard-on-WSL-crashes-in-libudev-so/m-p/1189032
  runScript = writeScript "${name}-wrapper" ''
    exec "$@"
  '';
}
