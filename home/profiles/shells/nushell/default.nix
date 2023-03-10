{ ... }: {
  programs.nushell = {
    enable = true;
    configFile.text = ''
      let $config = {
        filesize_metric: false
        table_mode: rounded
        use_ls_colors: true
      }
    '';
  };

  programs.keychain.enableNushellIntegration = true;
}
