{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    zoom-us # 🎵 https://www.youtube.com/watch?v=_yF3fCWfmEk 🎵
    pkgs.nur.repos.clefru.ib-tws # Interactive Brokers TWS
  ];
}
