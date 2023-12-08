{ config, lib, pkgs, ... }:
let
  inherit (config.lib.dotfield.whoami)
    email fullName githubUserName pgpPublicKey;

  inherit (config.lib) dotfield;
  configDir = dotfield.userConfigPath + "/git";

  enableSigning = config.programs.gpg.enable && config.services.gpg-agent.enable
    && "" != pgpPublicKey;

  git-gpg-privacy = with pkgs;
    writeShellScriptBin "git-gpg-privacy" ''
      # Epoch for today at 00:00:00
      EPOCH_TODAY="$(date --date=$(date --iso-8601=date) +'%s')"

      ${gnupg}/bin/gpg2 --faked-system-time "$EPOCH_TODAY!" $@
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
    gitAndTools.git-review # : tool to submit code reviews to Gerrit
    gitAndTools.git-big-picture # : visualize Git repositories
    nodePackages.git-run # : 'gr': a tool for managing multiple git repositories (github:mixu/gr)

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
  ] ++ lib.optionals config.programs.password-store.enable [
    pass-git-helper
  ];

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    # Enable Large File Storage.
    lfs.enable = true;

    userEmail = email;
    userName = fullName;

    signing = lib.mkIf enableSigning {
      key = pgpPublicKey;
      signByDefault = true;
      gpgPath = "${git-gpg-privacy}/bin/git-gpg-privacy";
    };
    includes =
      let
        personalIncludes = dir: {
          condition = "gitdir:${dir}";
          contents = {
            user.email = "git@cfeeley.org";
            # What I do on my own time is mine. Keep yer' grubby mitts off my timestamps!
            privacy = {
              pattern = "hms";
              replacements = true;
              # "my own time" = 6PM - 9AM, apparently.
              limit = "18-9";
            };
          };
        };
        professionalIncludes = dir: {
          condition = "gitdir:${dir}";
          contents = {
            user.email = "cfeeley@rossvideo.com";
            # Professionals *totally* don't make their best contributions at 3AM... right?
            privacy = {
              pattern = "s";
              replacements = true;
              # ... not anymore! Automatically round it to 9 AM or 6PM (whichever is closer).
              limit = "9-18";
            };
          };
        };
      in
      [
        (personalIncludes "~/source/")
        (personalIncludes "~/.config/")
        (professionalIncludes "~/dev/")
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
      amend = "commit --amend";
      br = "branch";
      ci = "commit";
      co = "checkout";
      d = "diff";
      dc = "diff --cached";
      ds = "diff --staged";
      dump = "cat-file -p";
      hist = "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
      init-repo = "!git init && git commit --allow-empty --message 'Initial commit' && :";
      l = "log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      la = "log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative --all";
      latest = "for-each-ref --format='%(committerdate:iso8601) %(committerdate:relative) %(refname)' --sort -committerdate";
      lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
      lga = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative --all";
      s = "status -sb";
      st = "status";
      type = "cat-file -t";
      clone-for-worktrees =
        # https://morgan.cugerone.com/blog/workarounds-to-git-worktree-using-bare-repository-and-cannot-fetch-remote-branches/
        let
          git-clone-bare-for-worktrees =
            pkgs.writeShellScript "git-clone-bare-for-worktrees" ''
              set -e

              # Examples of call:
              # git-clone-bare-for-worktrees git@github.com:name/repo.git
              # => Clones to a /repo directory
              #
              # git-clone-bare-for-worktrees git@github.com:name/repo.git my-repo
              # => Clones to a /my-repo directory

              url=$1
              basename=''${url##*/}
              name=''${2:-''${basename%.*}}

              mkdir $name
              cd "$name"

              # Moves all the administrative git files (a.k.a $GIT_DIR) under .bare directory.
              #
              # Plan is to create worktrees as siblings of this directory.
              # Example targeted structure:
              # .bare
              # main
              # new-awesome-feature
              # hotfix-bug-12
              # ...
              git clone --bare "$url" .bare
              echo "gitdir: ./.bare" > .git

              # Explicitly sets the remote origin fetch so we can fetch remote branches
              git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"

              # Gets all branches from origin
              git fetch origin
            '';
        in
        "!sh ${git-clone-bare-for-worktrees}";
    };

    attributes = [ ",*.org   diff=org" ]; # org-mode diffing

    extraConfig = lib.mkMerge [{
      diff.org.xfuncname = "^(\\*+ +.*)$"; # org-mode diffing

      init.defaultBranch = "master";
      github.user = githubUserName;

      # Environment variables will not be expanded -- this requires a path.
      init.templateDir = "${dotfield.userConfigPath}/git/templates";

      # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
      pretty.nice =
        "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

      gitlab."srvvirgitlab.rossvideo.com/api/v4" = {
        user = "cfeeley";
      };

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

      privacy = {
        pattern = "hms";
        replacements = true;
        # "my own time" = 6PM - 9AM, apparently.
        limit = "18-9";
      };
      ##: }}
    }
      (lib.mkIf pkgs.stdenv.isDarwin {
        # Not supported on NixOS
        core.fsmonitor = true;
        fsmonitor.socketDir = "${configDir}/fsmonitor";
      })];
  };

  programs.gh = {
    enable = true;
    extensions = with pkgs; [ gh-dash ];
  };

  xdg.configFile."git/templates".source = "${configDir}/templates";
}
