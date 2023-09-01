{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv) hostPlatform;
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin isAarch64;
  inherit (config.xdg) configHome;
  inherit (config.lib.dag) entryAfter;
  inherit (config.lib.file) mkOutOfStoreSymlink;

  emacsPackage =
    let
      emacs-pkg = with pkgs;
        if isDarwin
        then
          emacs29-macport.overrideAttrs
            (old: {
              # Required for 'hammy' emacs package
              buildInputs = old.buildInputs ++ [ pkgs.dbus ];
              nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.darwin.apple_sdk.frameworks.Accelerate ];

              src = pkgs.fetchFromBitbucket {
                owner = "mituharu";
                repo = "emacs-mac";
                rev = "719b04b75fd30f39d2de973081036784a9fbc8f2"; # 2023-08-28: tracking 'work' branch
                hash = "sha256-mRBhYmhBDXWUZn8soVEfEWLtudDBSmZgZ0zNxQQ8yW0=";
              };
            })
        #: isLinux: emacs 29 (w/ native comp)
        else
          pkgs.emacs29.override {
            inherit (pkgs)
              # For withGTK3:
              gtk3-x11 gsettings-desktop-schemas
              # For withXwidgets:
              webkitgtk wrapGAppsHook glib-networking;

            withGTK3 = false;
            withXwidgets = false;
            withSQLite3 = true;
            withWebP = true;
          };
    in
    (emacs-pkg.override { }).overrideAttrs (old: {
      dontStrip = true;
      separateDebugInfo = true;


      # Compile native ELisp files ahead of time (emacs 29+)
      configureFlags = old.configureFlags ++ lib.optional (lib.versionAtLeast old.version "29") [ "--with-native-compilation=aot" ];

      patches = old.patches ++ [
        # Reduce wall clock latency for sweep_conses by 50%
        #   https://tdodge.consulting/blog/living-the-emacs-garbage-collection-dream
        (pkgs.fetchpatch {
          url = "https://github.com/tyler-dodge/emacs/commit/36d2a8d5a4f741ae99540e139fff2621bbacfbaa.patch";
          sha256 = "sha256-/hJa8LIqaAutny6RX/x6a+VNpNET86So9xE8zdh27p8=";
        })

        # Process output from subprocesses continually
        # (avoids 1024 byte bottleneck with subprocess output on MacOS)
        #   https://tdodge.consulting/blog/eshell/background-output-thread
        # (pkgs.fetchpatch {
        #   url = "https://github.com/tyler-dodge/emacs/commit/b386047f311af495963ad6a25ddda128acc1d461.patch";
        #   sha256 = "sha256-dRkiowEtu/oOLh29/b7VSXGKsV5qE0PxMWrox5/LRoM=";
        # })
      ];
    });

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  doomRepoRev = "e96624926d724aff98e862221422cd7124a99c19";

  emacsDir = "${configHome}/emacs";

  e-wrapper = pkgs.writeShellScriptBin "e"
    (builtins.readFile "${config.lib.dotfield.srcPath}/config/emacs/e");

  doom-corfu = pkgs.fetchgit {
    url = "https://git.sr.ht/~gagbo/doom-config";
    rev = "bd03bcb6a84d913acf3d4590b7ec5120b335be1e";
    sha256 = "sha256-5j4zz4OnS+kewmJ9QRTuE2qkQCSL3Q9NbVLIu8FUiLI=";

    fetchSubmodules = false;
    sparseCheckout = [ "modules/completion/corfu" ];
  };

  llvmPackages = pkgs.llvmPackages_14;
  clang-tools = pkgs.clang-tools.override { inherit llvmPackages; };
in
lib.mkMerge [
  {
    home.sessionVariables = {
      EMACSDIR = emacsDir;

      # "default" profile
      # FIXME: profiles seem broken, see doom issue tracker
      # DOOMPROFILE = "doom";

      # NOTE: doom is picky about having the trailing slash
      DOOMDIR = "${configHome}/doom/";

      # local state :: built files, dependencies, etc.
      # TODO: may no longer be necessary with doom profiles. re-evaluated.
      # DOOMLOCALDIR = doomStateDir;

      # lsp: use plists instead of hashtables for performance improvement
      # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
      LSP_USE_PLISTS = "true";
    };

    home.sessionPath = [ "${configHome}/emacs/bin" ];

    xdg.configFile."doom/modules/completion/corfu".source =
      mkOutOfStoreSymlink "${doom-corfu}/modules/completion/corfu";

    ## Doom Bootloader.
    #: <https://github.com/doomemacs/doomemacs/commit/5b6b204bcbcf69d541c49ca55a2d5c3604f04dad>
    # FIXME: profiles seem broken
    # xdg.configFile."emacs/profiles/doom".source =
    #   mkOutOfStoreSymlink "${profilesPath}/doom";
    # xdg.configFile."emacs/profiles/xtallos".source =
    #   mkOutOfStoreSymlink "${profilesPath}/xtallos";

    # FIXME: use doom profile loader once issues are fixed upstream
    # xdg.configFile."doom".source =
    #   mkOutOfStoreSymlink "${profilesPath}/doom";

    # Install Doom imperatively to make use of its CLI.
    # While <github:nix-community/nix-doom-emacs> exists, it is not recommended
    # due to the number of oddities it introduces.
    # home.activation.installDoomEmacs =
    #   let
    #     git = "$DRY_RUN_CMD ${pkgs.git}/bin/git";
    #   in
    #   entryAfter [ "writeBoundary" ] ''
    #     if [[ ! -f "${emacsDir}/README.md" ]]; then
    #       [[ ! -d "${emacsDir}" ]] && mkdir "${emacsDir}"
    #       cd ${emacsDir}
    #       ${git} init --initial-branch master
    #       ${git} remote add origin ${doomRepoUrl}
    #       ${git} fetch origin master
    #       ${git} reset --hard origin/master
    #     fi

    #     # Checkout pinned SHA
    #     ${git} -C ${emacsDir} fetch
    #     ${git} -C ${emacsDir} reset --hard ${doomRepoRev}
    #   '';

    programs.emacs = {
      enable = true;
      package = emacsPackage;
      extraPackages = epkgs:
        with epkgs; [
          vterm
          pdf-tools
          org-pdftools
          tree-sitter
          (tree-sitter-langs.withPlugins
            (_: pkgs.tree-sitter.allGrammars))
          tsc
          parinfer-rust-mode
          eldoc
          eldoc-box
          eglot
          consult-eglot
          flycheck-eglot
        ];
    };

    services.emacs = lib.mkIf (!hostPlatform.isDarwin) {
      enable = lib.mkDefault true;
      defaultEditor = lib.mkForce true;
      socketActivation.enable = false;
      startWithUserSession =
        lib.mkDefault true; # implies socketActivitaion is disabled
      client.enable =
        lib.mkDefault false; # Don't generate desktop file - just use e-wrapper
    };

    home.packages = with pkgs;
      [
        python3Packages.pylatexenc

        # Emacsclient wrapper
        e-wrapper

        ediff-tool
        gnutls
        (ripgrep.override { withPCRE2 = true; })

        fd # faster projectile indexing
        imagemagick # for image-dired and emacs-gif-screencast
        gifsicle
        zstd # for undo-fu-session/undo-tree compression
        feh

        figlet # prettier block comments

        #: vterm
        cmake

        #: org
        graphviz
        gnuplot
        plantuml

        # :lang lua
        luaPackages.lua-lsp

        #: parinfer
        parinfer-rust
        # emacs == vim... at least as far as the required parinfer library package is concerned
        vimPlugins.parinfer-rust

        # :lang latex & :lang org (latex previews)
        texlive.combined.scheme-medium
        groff
        # :lang nix
        nixpkgs-fmt

        # :lang sh
        shellcheck # <- bash doesn't have to be scary
        nodePackages.bash-language-server # <- lsp client for bash that loves dividing by zero
        # Linux-only: bashdb # <- bash debugger
        # zshdb # <- zsh debugger (FIXME: not packaged for nix)

        # :lang cpp
        # NOTE: lldb-14 is broken
        llvmPackages_13.lldb # includes lldb-vscode
        clang-tools
        # Linux-only (see conditional appends below):
        # (vscode-extensions.ms-vscode.cpptools.override { inherit clang-tools; })

        # :tools magit
        gitAndTools.delta

        # :tools pdf
        # Use with (package! pdf-tools :built-in 'prefer)
        emacsPackage.pkgs.pdf-tools

        # Treemacs
        python3

        # Fonts
        emacs-all-the-icons-fonts

        # FIXME: sqlite binary unusable in org-roam and forge even after supplying
        # them... so we let these packages compile the binary...
        stdenv.cc
        sqlite

        editorconfig-core-c

        ##: === writing ===

        # :checkers spell
        (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
        (hunspellWithDicts (with hunspellDicts; [ hunspellDicts.en-ca-large ]))
        languagetool

        # :tools lookup
        wordnet

        # :tools copilot
        nodejs-16_x # Copilot requires Node.js version 17 or below

        ##: === lang/lsp ===

        #: docker
        nodePackages.dockerfile-language-server-nodejs
        #: terraform
        terraform
        terraform-ls
        #: HTML/CSS/JSON/ESLint
        nodePackages.vscode-langservers-extracted
        #: css
        nodePackages.vscode-css-languageserver-bin
        #: js
        nodePackages.eslint
        nodePackages.typescript-language-server
        #: json
        nodePackages.vscode-json-languageserver
        #: markdown
        nodePackages.unified-language-server
        #: python
        pyright
        #: C++
        clang-tools
        bear
        #: nix
        rnix-lsp
        nil # ('nix-nil' from source repo)
        nixd
        #: toml
        taplo-lsp
        #: web-mode
        nodePackages.js-beautify
        nodePackages.stylelint
        nodePackages.vscode-html-languageserver-bin
        html-tidy
        #: yaml
        nodePackages.yaml-language-server
        #: vimrc
        nodePackages.vim-language-server

        #: lang rescript
        # FIXME(2023-07-27): version 3e9 of dune is not supported
        # rescript
        # (pkgs.writeShellApplication {
        #   name = "rescript-language-server";
        #   runtimeInputs = [ nodejs vscode-extensions.chenglou92.rescript-vscode ];
        #   text = ''
        #     exec ${nodejs}/bin/node ${vscode-extensions.chenglou92.rescript-vscode}/share/vscode/extensions/chenglou92.rescript-vscode/server/out/server.js "$@"
        #   '';
        # })
        # nodejs

        #: lang reasonml
        vscode-extensions.freebroccolo.reasonml
        ocamlPackages.merlin
        ocamlPackages.lsp
      ] ++ (lib.optionals (isLinux && !isAarch64) [
        scrot
        peek

        # :lang sh
        bashdb # <- bash debugger

        #: lang cpp
        (vscode-extensions.ms-vscode.cpptools.override { inherit clang-tools; })
        cmake-language-server

        # :lang python
        python3Packages.debugpy

        #: lang graphql
        nodePackages.graphql-language-service-cli

        #: fpga (bazel builds fail on darwin)
        verible
        verilator
        svlangserver
        svls
        svlint

        # XWidgets WebKit
        webkitgtk
        glib
        gtk3
        glib-networking
        gsettings-desktop-schemas

        # For emacs-everywhere
        xorg.xwininfo
        xdotool
        xclip
      ]);

    # Configure aspell
    xdg.configFile."aspell/aspell.conf" = {
      text = ''
        master en_US
        extra-dicts en-computers.rws
        add-extra-dicts en_US-science.rws
      '';
      executable = false;
    };
  }
  (lib.mkIf isLinux {
    # Register org-protocol as a handler for 'org-protocol://' links
    xdg.desktopEntries.org-protocol = {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org protocol";
      genericName = "org-protocol";
      type = "Application";
      mimeType = [ "x-scheme-handler/org-protocol" ];
      noDisplay = true; # Register handler, but don't add application to menus
    };
  })
]
