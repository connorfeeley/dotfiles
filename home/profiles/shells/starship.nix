{ config
, lib
, pkgs
, ...
}:

let
  mkPrompt = sep: arr: lib.concatMapStrings (x: "$" + x) arr;
in
{
  programs.starship = {
    enable = true;

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
        "character"
      ];

      character = {
        success_symbol = "[λ](bold purple)";
        error_symbol = "[!](bold red)";
        vicmd_symbol = "[❮](bold white)";
      };
      cmd_duration = {
        show_notifications = false; # FIXME: Hangs starship when enabled
      };

      directory.style = "blue";

      command_timeout = 5000;
      add_newline = false;
      line_break.disabled = false;
    };
  };
}
