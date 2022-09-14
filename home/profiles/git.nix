{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) getAttr attrNames;
  inherit
    (config.lib.dotfield.whoami)
    email
    fullName
    githubUserName
    pgpPublicKey
    ;

  enableSigning =
    config.programs.gpg.enable
    && config.services.gpg-agent.enable
    && "" != pgpPublicKey;

  git-gpg-privacy = with pkgs; writeShellScriptBin "git-gpg-privacy" ''
    # Epoch for today at 00:00:00
    EPOCH_TODAY="$(date --date=$(date --iso-8601=date) +'%s')"

    ${gnupg}/bin/gpg2 --faked-system-time "$EPOCH_TODAY!" --no-emit-version $@
  '';
in {
  home.packages = with pkgs; [
    ediff-tool
    exiftool # EXIF diff handler
    git-cliff
    git-submodule-rewrite
    gitAndTools.hub
    gitAndTools.gh
    gitAndTools.tig
    gitAndTools.git-crypt

    # Identify the largest files in a git repo's history.
    #
    # Even after committing the deletion of a file, it will remain in git
    # history forever. This script allows for the identification of such files,
    # sorted from smallest to largest.
    #
    # via: https://stackoverflow.com/a/42544963
    (writeShellScriptBin "git-hls-by-size" ''
      git rev-list --objects --all \
        | git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' \
        | sed -n 's/^blob //p' \
        | sort --numeric-sort --key=2 \
        | cut -c 1-12,41- \
        | $(command -v gnumfmt || echo numfmt) --field=2 --to=iec-i --suffix=B --padding=7 --round=nearest
    '')

    # Privacy
    gitAndTools.git-privacy
    git-gpg-privacy
  ];

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = email;
    userName = fullName;

    signing = lib.mkIf enableSigning {
      key = pgpPublicKey;
      signByDefault = true;
    };

    difftastic.enable = true;

    delta = {
      enable = false;
      options = {
        # Breaks magit-delta when enabled
        line-numbers = false;

        navigate = true;
        keep-plus-minus-markers = true;
      };
    };

    ignores = [
      ".yarn"
      "node_modules"

      # Logs and databases
      "*.sql"
      "*.sqlite"
      ".log"

      # OS or Editor files
      "._*"
      ".DS_Store"
      ".DS_Store?"
      "ehthumbs.db"
      "Thumbs.db"
      ".tern-project"

      # Files that might appear on external disks
      ".Spotlight-V100"
      ".Trashes"

      # Always-ignore extensions
      "*~"
      "*.err"
      "*.orig"
      "*.pyc"
      "*.rej"
      "*.sw?"
      "*.vi"
      "*.bak"

      # Credentials and Sensitive Info
      ".direnv"
      ".scratch"
      "*localrc"
      "*.local"

      # Direnv
      ".envrc"
    ];

    extraConfig = lib.mkMerge [
      {
        init.defaultBranch = "master";
        github.user = githubUserName;

        # Environment variables will not be expanded -- this requires a path.
        init.templateDir = "${config.xdg.configHome}/git/templates";

        # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
        pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

        ##: Shorthands {{
        url."https://github.com/" = {
          insteadOf = "github:";
          pushInsteadOf = "github:";
        };
        url."git@git.sr.ht:" = {
          insteadOf = "srht:";
          pushInsteadOf = "srht:";
        };
        ##: }}
        ##: Remotes {{
        fetch.recurseSubmodules = true;
        push.default = "current";
        apply.whitespace = "nowarn";
        # Only enable this on a per-repo basis.
        pull.rebase = false;
        ##: }}

        ##: Diff/Merge Tools {{
        rerere.enabled = true;
        merge.conflictstyle = "diff3";
        merge.tool = "ediff";

        diff = {
          algorithm = "minimal";
          exif.textconv = "${pkgs.exiftool}/bin/exiftool";
          # colorMoved = "default";
          tool = "ediff";
          # `plutil` is a darwin utility
          plist.textconv = "plutil -convert xml1 -o -";
        };

        difftool = {
          prompt = false;
          ediff.cmd = "${pkgs.ediff-tool}/bin/ediff-tool $LOCAL $REMOTE";
          vscode.cmd = "code --wait --diff $LOCAL $REMOTE";
        };

        mergetool = {
          prompt = false;
          ediff.cmd = "${pkgs.ediff-tool}/bin/ediff-tool $LOCAL $REMOTE $MERGED";
          vscode.cmd = "code --wait $MERGED";
        };
        ##: }}
      }

      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        # TODO: still necessary?
        credential.helper = "osxkeychain";
      })
    ];
  };

  programs.gh.enable = true;
  programs.gh.settings.git_protocol = "ssh";

  xdg.configFile."git/templates".source = "${pkgs.dotfield-config}/git/templates";
}
