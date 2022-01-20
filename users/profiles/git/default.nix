{ config, lib, pkgs, ... }:

let
  inherit (config) my;
  inherit (lib) getAttr attrNames;

  configDir = "${config.dotfield.configDir}/git";

  scripts = with pkgs; {
    submoduleRewrite = (writeScriptBin "git-submodule-rewrite"
      (builtins.readFile "${configDir}/bin/git-submodule-rewrite"));
  };

  userScripts = (builtins.map
    (key: getAttr key scripts)
    (attrNames scripts));


  ediffTool = "${pkgs.dotfield.ediffTool}/bin/ediff-tool";

in
{
  my.env = { GIT_EDITOR = "$EDITOR"; };

  my.user.packages = with pkgs; [
    gitAndTools.hub
    gitAndTools.gh
    gitAndTools.tig
    # TODO: this probably doesn't belong in this profile
    universal-ctags
    # For EXIF diff handling
    exiftool
  ] ++ userScripts;

  my.hm.programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = my.email;
    userName = my.name;

    signing = {
      key = my.keys.pgp;
      signByDefault = true;
    };

    delta = {
      enable = true;
      options = {
        line-numbers = true;
        navigate = true;
      };
    };

    extraConfig = {
      github.user = my.githubUsername;

      init.defaultBranch = "main";
      # Environment variables will not be expanded -- this requires a path.
      init.templateDir = "${my.xdg.config}/git/templates";

      # TODO: handled by the delta hm module?
      # core.pager = "delta";
      # TODO: useful? not useful?
      # help.autocorrect = false;

      # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
      pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

      ## Remotes {{
      fetch.recurseSubmodules = true;
      pull.rebase = true;
      push.default = "current";
      apply.whitespace = "nowarn";
      ## }}

      ## Diff/Merge Tools {{
      rerere.enabled = true;
      merge.conflictstyle = "diff3";
      # TODO: does the delta hm module handle this?
      # interactive.diffFilter = "delta --color-only";
      diff = {
        exif.textconv = "${pkgs.exiftool}/bin/exiftool";
        # colorMoved = "default";
        tool = "ediff";
        # `plutil` is a darwin utility
        plist.textconv = "plutil -convert xml1 -o -";
      };
      merge.tool = "ediff";
      difftool = {
        prompt = false;
        ediff.cmd = "${ediffTool} $LOCAL $REMOTE";
        vscode.cmd = "code --wait --diff $LOCAL $REMOTE";
      };
      mergetool = {
        prompt = false;
        ediff.cmd = "${ediffTool} $LOCAL $REMOTE $MERGED";
        vscode.cmd = "code --wait $MERGED";
      };
      ## }}

    } // (if pkgs.stdenv.isDarwin then {
      credential.helper = "osxkeychain";
    } else { });
  };

  my.hm.xdg.configFile = {
    "git/ignore".source = "${configDir}/ignore";

    # TODO: disabled but not forgotten...
    # FIXME: next time we enable a global hook template, consider linking only the file on a host-by-host basis
    # "git/templates" = {
    #   source = "${configDir}/templates";
    #   recursive = true;
    # };
  };
}
