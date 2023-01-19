{ config
, lib
, pkgs
, ...
}:
let
  inherit
    (config.lib.dotfield.whoami)
    email
    fullName
    githubUserName
    pgpPublicKey
    ;

  inherit (config.lib) dotfield;
  configDir = "${dotfield.userConfigPath}/git";

  enableSigning =
    config.programs.gpg.enable
    && config.services.gpg-agent.enable
    && "" != pgpPublicKey;

  git-gpg-privacy = with pkgs; writeShellScriptBin "git-gpg-privacy" ''
    # Epoch for today at 00:00:00
    EPOCH_TODAY="$(date --date=$(date --iso-8601=date) +'%s')"

    ${gnupg}/bin/gpg2 --faked-system-time "$EPOCH_TODAY!" --no-emit-version $@
  '';
in
{
  home.packages = with pkgs; [
    ediff-tool
    exiftool # EXIF diff handler
    git-cliff
    git-filter-repo # Replaces git-filter-branch
    bfg-repo-cleaner # Like git-filter-repo
    git-sizer # Analyze shape of repository
    git-submodule-rewrite
    gitAndTools.hut # Sourcehut CLI
    gitAndTools.hub # GitHub CLI
    gitAndTools.glab # GitLab CLI
    gitAndTools.lab # Another GitLab CLI
    gitAndTools.tig
    gitAndTools.git-crypt
    gitAndTools.git-standup
    gitAndTools.git-review #: tool to submit code reviews to Gerrit
    gitAndTools.git-big-picture #: visualize Git repositories
    nodePackages.git-run #: 'gr': a tool for managing multiple git repositories (github:mixu/gr)

    # # Other potentially interesting packages:
    # gitAndTools.git-machete #: github:VirtusLab/git-machete
    # gitAndTools.gita #: github:nosarthur/gita
    # gitAndTools.gitleaks #: github:zricethezav/gitleaks

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
    includes = [
      # Professionals *totally* don't make their best contributions at 3AM... right?
      {
        condition = "gitdir:~/dev/**";
        contents = {
          user.email = "cfeeley@rossvideo.com";
          privacy = {
            pattern = "s";
            replacements = true;
            # ... not anymore! Automatically round it to 9 AM or 6PM (whichever is closer).
            limit = "9-18";
          };
        };
      }
      # What I do on my own time is mine. Keep yer' grubby mitts off my timestamps.
      {
        condition = "gitdir:~/source/**";
        contents = {
          user.email = "git@cfeeley.org";
          privacy = {
            pattern = "hms";
            replacements = true;
            # "my own time" = 6PM - 9AM, apparently.
            limit = "18-9";
          };
        };
      }
    ];

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

    aliases = {
      co = "checkout";
      ci = "commit";
      st = "status";
      br = "branch";
      latest = "for-each-ref --format='%(committerdate:iso8601) %(committerdate:relative) %(refname)' --sort -committerdate";
      hist = "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
      type = "cat-file -t";
      dump = "cat-file -p";
      lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      lga = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative --all";
      l = "log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      la = "log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative --all";
      s = "status -sb";
      d = "diff";
      dc = "diff --cached";
      ds = "diff --staged";
      amend = "commit --amend";
    };

    extraConfig = lib.mkMerge [
      {
        init.defaultBranch = "master";
        github.user = githubUserName;

        # Environment variables will not be expanded -- this requires a path.
        init.templateDir = "${dotfield.userConfigPath}/git/templates";

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
        url."git@srvvirgitlab.rossvideo.com:connectivity-sw/" = {
          insteadOf = "srvvirgitlab:";
          pushInsteadOf = "srvvirgitlab:";
        };
        ##: }}
        ##: Remotes {{
        fetch.recurseSubmodules = true;
        push.default = "current";
        apply.whitespace = "nowarn";
        # Only enable this on a per-repo basis.
        pull.rebase = false;
        ##: }}

        ##: Misc {{
        # Opt-in to "features that make git the smoothest it can be"
        # https://github.blog/2019-11-03-highlights-from-git-2-24/#feature-macros
        feature.manyFiles = true;
        ##: }}

        ##: Maintenance {{
        maintenance = {
          auto = true;
          strategy = "incremental";
        };
        ##: }}

        ##: rerere {{
        rerere = {
          enabled = true;
          autoUpdate = true; # autostage files resolved by rerere
        };

        ##: Diff/Merge Tools {{
        merge = {
          conflictstyle = "diff3";
          tool = "ediff";
        };

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
    ];
  };

  programs.gh = {
    enable = true;
    extensions = with pkgs; [ gh-dash ];
  };

  xdg.configFile."git/templates".source = "${configDir}/templates";
}
