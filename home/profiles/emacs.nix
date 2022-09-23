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
  emacsDir = "${configHome}/emacs";

  # FIXME: this is gross
  configDir = ../../.;
  e-wrapper = pkgs.writeShellScriptBin "e" (builtins.readFile "${configDir}/config/emacs/e");

  doom-emacs-rev = "e9ef904eeef28c31ca07055f0cce8c5eafeb4179";
  doom-corfu = pkgs.fetchgit {
    url = "https://git.sr.ht/~gagbo/doom-config";
    rev = "583648e00f2b6e23c6b99e41ed896cd76bf6eaf0";
    sha256 = "sha256-O7HochHdpJVviP4ioN5uco+AoBAWwghPDDMBe4umK0I=";

    fetchSubmodules = false;
    sparseCheckout = "modules/completion/corfu";
  };
in
{
  home.sessionVariables = {
    EMACSDIR = emacsDir;

    # "default" profile
    # FIXME: profiles seem broken, see doom issue tracker
    # DOOMPROFILE = "doom";

    DOOMDIR = "${configHome}/doom";

    # local state :: built files, dependencies, etc.
    # TODO: may no longer be necessary with doom profiles. re-evaluated.
    # DOOMLOCALDIR = doomStateDir;

    # lsp: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  home.sessionPath = [ "${configHome}/emacs/bin" "$PATH" ];

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
      git -C ${emacsDir} checkout ${doom-emacs-rev}
    '';

  programs.emacs = {
    enable = true;
    # package = pkgs.emacs;
    package =
      let
        # x86_64-linux: emacsPgtkNativeComp
        # aarch-linux: emacsNativeComp
        # aarch-darwin: emacsPgtkNativeComp
        pkg =
          if (isLinux && !isAarch64)
          then pkgs.emacsPgtkNativeComp
          else pkgs.emacsNativeComp;
      in
      pkg;
    extraPackages = epkgs: with epkgs; [
      vterm
      pdf-tools
      tree-sitter
      tree-sitter-langs
      tsc
    ];
  };

  services.emacs = lib.mkIf (!hostPlatform.isDarwin) {
    # Doom will take care of running the server.
    enable = lib.mkDefault false;
    defaultEditor = lib.mkForce true;
    socketActivation.enable = false;
  };

  home.packages = with pkgs; [
    # Emacsclient wrapper
    e-wrapper

    ediff-tool
    gnutls
    (ripgrep.override { withPCRE2 = true; })

    fd # faster projectile indexing
    imagemagick # for image-dired and emacs-gif-screencast
    scrot
    gifsicle
    peek
    zstd # for undo-fu-session/undo-tree compression

    figlet # prettier block comments

    #: org
    graphviz

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
