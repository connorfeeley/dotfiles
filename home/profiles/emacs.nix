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
        #: isDarwin: emacs28Macport with native compilation from this repo (*IMPURE*)
        then
          emacsGitDarwin.overrideAttrs
            (old: {
              # Required for 'hammy' emacs package
              buildInputs = old.buildInputs ++ [ pkgs.dbus ];
            })
        #: isLinux: emacs 28 (w/ native comp)
        else
          pkgs.emacsGit.override {
            inherit (pkgs)
              # For withGTK3:
              gtk3-x11 gsettings-desktop-schemas
              # For withXwidgets:
              webkitgtk wrapGAppsHook glib-networking;

            withXwidgets = false;
            withGTK3 = false;
          };
    in
    (emacs-pkg.override {
      withSQLite3 = true;
      withWebP = true;
    }).overrideAttrs (old: { separateDebugInfo = true; });

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
          (epkgs.tree-sitter-langs.withPlugins
            (_: pkgs.tree-sitter.allGrammars))
          tsc
          parinfer-rust-mode
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

        # :lang lua
        luaPackages.lua-lsp

        #: parinfer
        parinfer-rust
        # emacs == vim... at least as far as the required parinfer library package is concerned
        vimPlugins.parinfer-rust

        # :lang latex & :lang org (latex previews)
        texlive.combined.scheme-medium
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

        ### ChatGPT
        # Requires epc and github:mmabrouk/chatgpt-wrapper
        python3Packages.chatgpt-wrapper
        (pkgs.writeShellScriptBin "python-chatgpt-wrapper" ''
          export PLAYWRIGHT_BROWSERS_PATH=${chatgpt-wrapper.playwrightBrowsers}
          exec -a $0 ${
            pkgs.python3.withPackages (ps: with ps; [ epc chatgpt-wrapper ])
          }/bin/python $@
        '')

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
        #: sh
        nodePackages.bash-language-server
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
