{ pkgs
, ...
}: {
  environment.systemPackages = with pkgs; [ zoom-us ];
}
