{ authFile }:

{
  containers.media-dl = {
    services.openvpn.servers = {
      pia = {
        autoStart = false;
        # Most of these options came from the OVPN file from the generator
        config = ''
          client
          dev tun
          proto udp
          remote ca-toronto.privacy.network 1197
          resolv-retry infinite
          nobind
          persist-key
          persist-tun
          cipher aes-256-cbc
          auth sha256
          tls-client
          remote-cert-tls server

          auth-user-pass ${authFile}
          compress
          verb 1
          reneg-sec 0

          # These settings was included directly in the file from
          # the generator, but I moved them to external files.
          <crl-verify>
          ${./crl.pem}
          </crl-verify>

          <ca>
          ${./ca.pem}
          </ca>

          disable-occ
        '';
      };
    };
  };
}
