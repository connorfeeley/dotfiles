{ config
, lib
, pkgs
, ...
}:

let
  mkPrompt = sep: arr: lib.concatMapStrings (x: "$" + x) arr;

  nerdTheme = {
    aws.symbol = "  ";
    buf.symbol = " ";
    conda.symbol = " ";
    dart.symbol = " ";
    directory.read_only = " ";
    docker_context.symbol = " ";
    elixir.symbol = " ";
    elm.symbol = " ";
    # git_branch.symbol = " ";
    golang.symbol = " ";
    haskell.symbol = " ";
    hg_branch.symbol = " ";
    java.symbol = " ";
    julia.symbol = " ";
    memory_usage.symbol = " ";
    nim.symbol = " ";
    nix_shell.symbol = " ";
    nodejs.symbol = " ";
    package.symbol = " ";
    python.symbol = " ";
    rust.symbol = " ";
  };

  lambdaTheme = {
    aws = {
      format =
        "\\[[$symbol($profile)(\\($region\\))(\\[$duration\\])]($style)\\]";
    };

    # c.format = "\\[[$symbol($version(-$name))]($style)\\]";

    cmake.format = "\\[[$symbol($version)]($style)\\]";
    cmd_duration.format = "\\[[⏱ $duration]($style)\\]";
    cobol.format = "\\[[$symbol($version)]($style)\\]";
    conda.format = "\\[[$symbol$environment]($style)\\]";
    crystal.format = "\\[[$symbol($version)]($style)\\]";
    dart.format = "\\[[$symbol($version)]($style)\\]";
    deno.format = "\\[[$symbol($version)]($style)\\]";
    docker_context.format = "\\[[$symbol$context]($style)\\]";
    dotnet.format = "\\[[$symbol($version)(🎯 $tfm)]($style)\\]";
    elixir.format = "\\[[$symbol($version (OTP $otp_version))]($style)\\]";
    elm.format = "\\[[$symbol($version)]($style)\\]";
    erlang.format = "\\[[$symbol($version)]($style)\\]";
    gcloud.format = "\\[[$symbol$account(@$domain)(($region))]($style)\\]";
    git_branch.format = "\\[[$symbol$branch]($style)\\]";
    git_status.format = "(\\[[$all_status$ahead_behind\\]]($style))\\]";
    golang.format = "\\[[$symbol($version)]($style)\\]";
    haskell.format = "\\[[$symbol($version)]($style)\\]";
    helm.format = "\\[[$symbol($version)]($style)\\]";
    hg_branch.format = "\\[[$symbol$branch]($style)\\]";
    java.format = "\\[[$symbol($version)]($style)\\]";
    julia.format = "\\[[$symbol($version)]($style)\\]";
    kotlin.format = "\\[[$symbol($version)]($style)\\]";
    kubernetes.format = "\\[[$symbol$context( ($namespace))]($style)\\]";
    lua.format = "\\[[$symbol($version)]($style)\\]";
    memory_usage.format = "[$symbol[$ram( | $swap)]($style)\\]";
    nim.format = "\\[[$symbol($version)]($style)\\]";
    nix_shell.format = "\\[[$symbol$state( ($name))]($style)\\]";
    nodejs.format = "\\[[$symbol($version)]($style)\\]";
    ocaml.format =
      "\\[[$symbol($version)(($switch_indicator$switch_name))]($style)\\]";
    openstack.format = "\\[[$symbol$cloud(($project))]($style)\\]";
    package.format = "\\[[$symbol$version]($style)\\]";
    perl.format = "\\[[$symbol($version)]($style)\\]";
    php.format = "\\[[$symbol($version)]($style)\\]";
    pulumi.format = "\\[[$symbol$stack]($style)\\]";
    purescript.format = "\\[[$symbol($version)]($style)\\]";
    python.format =
      "\\[[\${symbol}\${pyenv_prefix}(\${version})(($virtualenv))]($style)\\]";
    red.format = "\\[[$symbol($version)]($style)\\]";
    ruby.format = "\\[[$symbol($version)]($style)\\]";
    rust.format = "\\[[$symbol($version)]($style)\\]";
    scala.format = "\\[[$symbol($version)]($style)\\]";
    # spack.format = "\\[[$symbol$environment]($style)\\]";
    sudo.format = "\\[[as $symbol]\\]";
    swift.format = "\\[[$symbol($version)]($style)\\]";
    terraform.format = "\\[[$symbol$workspace]($style)\\]";
    time.format = "\\[[$time]($style)\\]";
    username.format = "\\[[$user]($style)\\]";
    vagrant.format = "\\[[$symbol($version)]($style)\\]";
    vlang.format = "\\[[$symbol($version)]($style)\\]";
    zig.format = "\\[[$symbol($version)]($style)\\]";
    command_timeout = 5000;
    add_newline = false;
    line_break.disabled = true;
  };

  piTheme = {
    command_timeout = 1000;
    add_newline = false;
    line_break = { disabled = true; };
    character = {
      success_symbol = "[π](bold gray)";
      error_symbol = "[π](bold red)";
      vicmd_symbol = "[V](bold green)";
    };
    directory = {
      style = "bold green";
      truncation_length = 3;
      truncation_symbol = "…/";
      truncate_to_repo = true;
      read_only = " ";
    };
    git_branch = { style = "bold purple"; };
    git_status = { style = "bold purple"; };
    package = { disabled = true; };
  };
in
{
  programs.starship = {
    enable = true;

    # Must be disabled for emacs-vterm integration to work.
    # Integration is handled manually in zsh.initExtra.
    enableZshIntegration = false;
    enableBashIntegration = false;

    settings = {
      # Prompt definition
      format = mkPrompt "$" [
        "username"
        "directory"
        "git_branch"
        "git_state"
        "git_status"
        "cmd_duration"
        "line_break"
        "python"
        "character"
        "hostname"
      ];

      character = {
        success_symbol = "[λ](bold purple)";
        error_symbol = "[!](bold red)";
        vicmd_symbol = "[❮](bold white)";
      };

      directory.style = "blue";

      command_timeout = 5000;
      add_newline = false;
      line_break.disabled = false;
    };
  };
}
