moduleArgs @ { config
, lib
, pkgs
, self
, ...
}:
let
  inherit (config.lib) dotfield;
  inherit (pkgs.stdenv) buildPlatform hostPlatform;
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin isAarch64;
  inherit (config.xdg) configHome;
  inherit (config.lib.dag) entryAfter;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.lib.dotfield.emacs) profilesBase profilesPath;

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  doomRepoRev = "ba35f12be43d4a93ea56cc5617a4a2543522e82e";

  emacsDir = "${configHome}/emacs";

  # FIXME: this is gross
  configDir = ../../.;
  e-wrapper = pkgs.writeShellScriptBin "e" (builtins.readFile "${configDir}/config/emacs/e");

  doom-corfu = pkgs.fetchgit {
    url = "https://git.sr.ht/~gagbo/doom-config";
    rev = "f9cbce9f84c69d55d3e7103b82e2c30cf101d62e";
    sha256 = "sha256-HGOkuBMLK5Xc0wPe8a8PjQIxcdPm/P+8CVkXOW0t16g=";

    fetchSubmodules = false;
    sparseCheckout = "modules/completion/corfu";
  };
in
lib.mkMerge [
  {
    home.sessionVariables = {
      # NOTE: trailing slash is
      EMACSDIR = emacsDir + "/";

      # "default" profile
      # FIXME: profiles seem broken, see doom issue tracker
      # DOOMPROFILE = "doom";

      # NOTE: trailing slash is
      DOOMDIR = "${configHome}/doom/";

      # local state :: built files, dependencies, etc.
      # TODO: may no longer be necessary with doom profiles. re-evaluated.
      # DOOMLOCALDIR = doomStateDir;

      # lsp: use plists instead of hashtables for performance improvement
      # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
      LSP_USE_PLISTS = "true";
    };

    home.sessionPath = [
      "${configHome}/emacs/bin"
    ];

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
    home.activation.installDoomEmacs =
      let
        git = "$DRY_RUN_CMD ${pkgs.git}/bin/git";
      in
      entryAfter [ "writeBoundary" ] ''
        if [[ ! -f "${emacsDir}/README.md" ]]; then
          [[ ! -d "${emacsDir}" ]] && mkdir "${emacsDir}"
          cd ${emacsDir}
          ${git} init --initial-branch master
          ${git} remote add origin ${doomRepoUrl}
          ${git} fetch origin master
          ${git} reset --hard origin/master
        fi

        # Checkout pinned SHA
        git -C ${emacsDir} fetch
        git -C ${emacsDir} reset --hard ${doomRepoRev}
      '';

    programs.emacs = {
      enable = true;
      package = with pkgs; if isDarwin
      then emacs28Macport # emacs28Macport with native compilation from this repo
      else emacsNativeComp;
      extraPackages = epkgs: with epkgs; [
        vterm
        pdf-tools
        tree-sitter
        tree-sitter-langs
        tsc
        parinfer-rust-mode
      ];
    };

    services.emacs = lib.mkIf (!hostPlatform.isDarwin) {
      # Doom will take care of running the server.
      enable = lib.mkDefault false;
      defaultEditor = lib.mkForce true;
      socketActivation.enable = false;
    };

    home.packages = with pkgs; [
      hunspell
      python3Packages.pylatexenc

      # Emacsclient wrapper
      e-wrapper

      # GPG-agent pinentry
      pinentry-emacs

      ediff-tool
      gnutls
      (ripgrep.override { withPCRE2 = true; })

      fd # faster projectile indexing
      imagemagick # for image-dired and emacs-gif-screencast
      scrot
      gifsicle
      peek
      zstd # for undo-fu-session/undo-tree compression
      feh

      figlet # prettier block comments

      #: vterm
      libvterm
      cmake
      gcc # required by libvterm

      #: org
      graphviz
      gnuplot

      #: parinfer
      parinfer-rust
      # emacs == vim... at least as far as the required parinfer library package is concerned
      vimPlugins.parinfer-rust

      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :tools magit
      gitAndTools.delta
      # :lang nix
      nixpkgs-fmt

      # Treemacs
      python3

      # Fonts
      emacs-all-the-icons-fonts

      # FIXME: sqlite binary unusable in org-roam and forge even after supplying
      # them... so we let these packages compile the binary...
      stdenv.cc
      sqlite

      editorconfig-core-c

      # Comment highling, namely for Doxygen in C++
      tree-sitter-grammars.tree-sitter-comment

      ##: === writing ===

      # :checkers spell
      (aspellWithDicts (ds:
        with ds; [
          en
          en-computers
          en-science
        ]))
      languagetool

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
      #: ledger
      # FIXME: marked as broken upstream
      # ledger
      #: markdown
      nodePackages.unified-language-server
      #: nix
      rnix-lsp
      nix-nil
      #: php
      # FIXME(darwin): broken
      nodePackages.intelephense
      #: ruby
      # FIXME(darwin): broken
      rubyPackages.solargraph
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
      #: fpga (bazel builds fail on darwin)
      verible
      verilator
      svlangserver
      svls
      svlint
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
  (lib.mkIf isLinux
    {
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
