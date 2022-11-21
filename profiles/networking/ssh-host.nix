{ config
, lib
, pkgs
, primaryUser
, collective
, ...
}: {
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;

  services.openssh = {
    enable = lib.mkForce true;

    ports = [ (lib.our.peers.getSshPort config.networking.hostName) ];
    permitRootLogin = lib.mkDefault "prohibit-password";
    forwardX11 = true;

    extraConfig = ''
      # Possibly fix hanging SSH multiplexes
      ClientAliveInterval 5

      StreamLocalBindUnlink yes
      AcceptEnv INSIDE_EMACS EMACS_VTERM_PATH
    '';
  };
  programs.mosh.enable = true;
}
