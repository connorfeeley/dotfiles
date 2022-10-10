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
        "git_state"
        "cmd_duration"
        "character"
      ];

      character = {
        success_symbol = "[λ](purple)";
        error_symbol = "[!](red)";
        vicmd_symbol = "[❮](white)";
      };

      directory.style = "blue";

      command_timeout = 5000;
      add_newline = false;
      line_break.disabled = false;
      username = {
        style_root = "red";
        style_user = "yellow";
      };
    };
  };
}
