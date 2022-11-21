{ lib
, peers
,
}:
lib.makeExtensible (self: rec {
  # treesWithValue :: (String -> Any -> Bool) -> [String] -> AttrSet -> [String]
  #
  # Given a nested attrset and an attribute path, return the names of the
  # top-level attributes whose attribute value at the given path matches a
  # predicate.
  #
  # One use case may be comparing all home-manager configurations (i.e. source
  # attrset) to determine whether a user (i.e. whose name may be returned in the
  # resulting list) has a matching value (i.e. predicate) within a particular
  # module (i.e. attrpath).
  #
  # Usage:
  #
  # let users = {
  #   foo = { services.gpg-agent.pinentryFlavor = "gtk2"; };
  #   bar = { services.gpg-agent.pinentryFlavor = "gnome3"; };
  # }; in
  # lib.treesWithValue (_: v: "gnome3" == v) ["services" "gpg-agent" "pinentryFlavor"] users
  # => [ "bar" ]
  treesWithValue = pred: path: attrs: (builtins.attrNames
    (lib.attrsets.filterAttrs
      (n: v: (pred n (lib.attrsets.getAttrFromPath path v)))
      attrs));

  # treesWithEnabledLeaf :: [String] -> AttrSet -> [String]
  #
  # Like `treesWithValue`, but with a pre-supplied no-op predicate to test a
  # boolean value only.
  #
  # let users = {
  #   foo = { programs.emacs.enable = true; };
  #   bar = { programs.emacs.enable = false; };
  # }; in
  # lib.treesWithEnabledLeaf ["programs" "emacs" "enable"] users
  # => [ "foo" ]
  treesWithEnabledLeaf = path: attrs: treesWithValue (_: v: v) path attrs;

  peers = rec {
    getHost = hostName: peers.hosts.${hostName} or false;
    getNet = network:
      if network != null
      then peers.networks.${network}
      else false;

    # If 'ssh_port' is set in hosts.toml for the supplied host, then use that; otherwise use port 22.
    getSshPort = hostName: (getHost hostName) . ssh_port or 22;
  };
})
