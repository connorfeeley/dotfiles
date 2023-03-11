{ pkgs, ... }: {
  home.packages = [
    pkgs.jc # convert output of popular CLI tools to JSON
  ];
  programs.nushell = {
    enable = true;
    configFile.text = ''
      let-env config = {
        rm_always_trash: true
        completion_algorithm: fuzzy
        show_banner: true

        filesize_metric: true
        table_mode: rounded
        use_ls_colors: true

        hooks : {
          pre_prompt: [{
            code: "
              let direnv = (direnv export json | from json)
              let direnv = if ($direnv | length) == 1 { $direnv } else { {} }
              $direnv | load-env
            "
          }]
        }
      }
    '';
  };

  programs.keychain.enableNushellIntegration = true;
}
