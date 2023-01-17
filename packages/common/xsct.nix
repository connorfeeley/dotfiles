{ lib
, stdenv
, fetchurl
, buildFHSUserEnv
, writeScript
, autoPatchelfHook
, pkgs
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

    # Skip unpack
    unpackPhase = ''echo "Skipping unpackPhase!"'';

    # Unpack directly into the output directory to save a bnuch of time on I/O
    installPhase = ''
      set -x

      du -hs $src

      mkdir -p $out
      tar -I 'xz -T0' -xf $src --strip-components=2 --exclude=gnu --exclude=data -C $out

      du -hs $out/*
      ls -al $out
      set +x
    '';

    # libidn.so.11 wanted by          $out/tps/lnx64/cmake-3.3.2/bin/ctest
    # libidn.so.11 wanted by          $out/tps/lnx64/cmake-3.3.2/bin/cpack
    # libidn.so.11 wanted by          $out/tps/lnx64/cmake-3.3.2/bin/ccmake
    # libidn.so.11 wanted by          $out/tps/lnx64/cmake-3.3.2/bin/cmake
    # libidn.so.11 wanted by          $out/tps/lnx64/cmake-3.3.2/bin/cmake-gui
    # ** libXext.so.6 wanted by          $out/tps/lnx64/cmake-3.3.2/bin/cmake-gui
    # ** libX11.so.6 wanted by           $out/tps/lnx64/cmake-3.3.2/bin/cmake-gui
    # libclang.so.3.9 wanted by       $out/lib/lnx64.o/libXidanePass.so
    # ** libX11.so.6 wanted by           $out/lib/lnx64.o/librdi_commonx11.so
    # libpython3.8.so.1.0 wanted by   $out/lib/lnx64.o/librdi_ipservicestasks.so
    # libpython3.8.so.1.0 wanted by   $out/lib/lnx64.o/librdi_project.so
    # ** libcrypt.so.1 wanted by         $out/bin/unwrapped/lnx64.o/xclkernelinfohash_new

    preFixup =
      let
        patchelfFixup = binary: needed: replaced: ''
          patchelf --replace-needed ${needed} ${replaced} ${binary} --output ${binary}.patched
          mv ${binary}.patched ${binary}
        '';
      in
      ''
        set -x

        # Add additional library paths to autoPatchelfHook's search path
        addAutoPatchelfSearchPath $out/lib

        ${patchelfFixup "$out/tps/lnx64/cmake-3.3.2/bin/ctest" "libidn.so.11" "libidn.so"}
        ${patchelfFixup "$out/tps/lnx64/cmake-3.3.2/bin/cpack" "libidn.so.11" "libidn.so"}
        ${patchelfFixup "$out/tps/lnx64/cmake-3.3.2/bin/ccmake" "libidn.so.11" "libidn.so"}
        ${patchelfFixup "$out/tps/lnx64/cmake-3.3.2/bin/cmake" "libidn.so.11" "libidn.so"}
        ${patchelfFixup "$out/tps/lnx64/cmake-3.3.2/bin/cmake-gui" "libidn.so.11" "libidn.so"}
        ${patchelfFixup "$out/lib/lnx64.o/libXidanePass.so" "libclang.so.3.9" "libclang.so"}
        ${patchelfFixup "$out/lib/lnx64.o/librdi_ipservicestasks.so" "libpython3.8.so.1.0" "libpython3.8.so"}
        ${patchelfFixup "$out/lib/lnx64.o/librdi_project.so" "libpython3.8.so.1.0" "libpython3.8.so"}

        substituteInPlace $out/bin/ldlibpath.sh \
            --replace "lsb_release $1 2>/dev/null | sed 's/^.*:[ 	]*//' | tr '[:upper:]' '[:lower:]'" "echo nixos-suse-nixos"

        set +x
      '';

    nativeBuildInputs = [ autoPatchelfHook ];

    buildInputs = with pkgs; [
      # common (autoPatchelfHook) requirements
      zlib
      xz
      llvmPackages.libclang
      libidn
      python38
      libxcrypt

      xorg.libXext
      xorg.libX11
    ];

    meta = with lib; {
      homepage = "https://www.xilinx.com/htmldocs/xilinx2019_1/SDK_Doc/xsct/intro/xsct_introduction.html";
      description = "Xilinx Software Command-Line Tools";
      license = licenses.unfree;
      maintainers = [ maintainers.cfeeley ];
      platforms = platforms.linux;
    };
  };
in
buildFHSUserEnv rec {
  name = "xsct-wrapper"; # wrapped

  targetPkgs = pkgs: with pkgs; [
    xsct

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
      executablesUnwrapped = [
        "aie_import"
        "buildinfo"
        "caller_rewrite"
        "cd_client_test"
        "cdoutil"
        "cf2bd"
        "cf2sw"
        "cf2xd"
        "cfgen"
        "cs_server"
        "diffbd"
        "emconfigutil"
        "evRouter"
        "flash_strip_bw"
        "gclkskewcheck"
        "HAOPResynthWorker"
        "hls_wrapper_gen"
        "hw_server"
        "hw_serverpv"
        "ipmetadata_config_checker"
        "ipmetadata_metadata_checker"
        "ipmetadata_schema_validator"
        "kernelinfo"
        "LCBEConfigCreator"
        "lgbmTest"
        "librabit.a"
        "lmgrd"
        "lmutil"
        "manage_ipcache"
        "messagepp"
        "mkfatimg"
        "parallel_synth_helper"
        "perf_analyze"
        "platforminfo"
        "pragma_gen"
        "prep_target"
        "prodversion"
        "rdi_program_ftdi"
        "rdi_vitis"
        "rdi_xilmfs"
        "rdi_xsct"
        "rdi_xsdb"
        "rdi_zynq_flash"
        "regiongen_new"
        "rlwrap"
        "srcscanner"
        "stub_gen"
        "svf_utility"
        "symbol_server"
        "system_link"
        "tcfgdbclient"
        "tcflog"
        "tclsh8.5"
        "test_model"
        "testSpeedModelController"
        "updatemem"
        "v++"
        "versal_noc_wizard"
        "vitis_analyzer"
        "vivado_lab"
        "vp_analyze"
        "vperf"
        "vpl"
        "vrs"
        "wbtcv"
        "xccrypt"
        "xccryptd"
        "xcd"
        "xclbinutil"
        "xclkernelinfohash_new"
        "xilcurl"
        "xilinxd"
        "xilpigz"
        "xilproxy"
        "xlicdiag"
        "xml2-config"
        "xrcserver"
        "xrflink"
        "xrt_server"
        "xvc_pcie"
      ];
      executablesStr = lib.concatStringsSep " " executablesUnwrapped;
    in
    ''
      mkdir -p $out/share/applications $out/share/icons/128x128
      ln -s ${xsct}/licenses/images/dc_quartus_panel_logo.png $out/share/icons/128x128/quartus.png

      ln -s ${xsct} $out/XSCT-DELETEME

      WRAPPER=$out/bin/${name}
      EXECUTABLES="${lib.concatStringsSep " " executablesUnwrapped}"
      for executable in $EXECUTABLES;
      do
        echo "#!${stdenv.shell}" >> $out/$executable
        echo "$WRAPPER ${xsct}/bin/unwrapped/lnx64.o/$executable \"\$@\"" >> $out/$executable
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
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${xsct}/lib/lnx64.o"
    echo "$LD_LIBRARY_PATH"
    exec "$@"
  '';
}
