# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub }:
{
  HPI = {
    pname = "HPI";
    version = "0.3.20220607";
    src = fetchurl {
      url = "https://pypi.io/packages/source/H/HPI/HPI-0.3.20220607.tar.gz";
      sha256 = "sha256-GME0Z+TH/6U+smFkuk1sT8UBzlySv5/yGhb42Kiaj8w=";
    };
  };
  fish-autopair = {
    pname = "fish-autopair";
    version = "1.0.4";
    src = fetchFromGitHub ({
      owner = "jorgebucaran";
      repo = "autopair.fish";
      rev = "1.0.4";
      fetchSubmodules = false;
      sha256 = "sha256-s1o188TlwpUQEN3X5MxUlD/2CFCpEkWu83U9O+wg3VU=";
    });
  };
  fish-fifc = {
    pname = "fish-fifc";
    version = "adff5966739667d4c13d6388372e40f821571208";
    src = fetchgit {
      url = "https://github.com/gazorby/fifc";
      rev = "adff5966739667d4c13d6388372e40f821571208";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-tUhEfwVtcd1iSHsmkOzkB5B33qK+x/AZ56Dgs8QEaDk=";
    };
    date = "2022-08-26";
  };
  fish-replay = {
    pname = "fish-replay";
    version = "1.2.1";
    src = fetchFromGitHub ({
      owner = "jorgebucaran";
      repo = "replay.fish";
      rev = "1.2.1";
      fetchSubmodules = false;
      sha256 = "sha256-bM6+oAd/HXaVgpJMut8bwqO54Le33hwO9qet9paK1kY=";
    });
  };
  fzf-scripts = {
    pname = "fzf-scripts";
    version = "15156e3cb56c715464a2421e6f4e4356a26ac975";
    src = fetchgit {
      url = "https://github.com/DanielFGray/fzf-scripts";
      rev = "15156e3cb56c715464a2421e6f4e4356a26ac975";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-rynePmia169HOvL0M2GTWrndulS6dKjfx7rT0GK9J0I=";
    };
    date = "2022-09-02";
  };
  hlissner-hey = {
    pname = "hlissner-hey";
    version = "089f1a9da9018df9e5fc200c2d7bef70f4546026";
    src = fetchgit {
      url = "https://github.com/hlissner/dotfiles";
      rev = "089f1a9da9018df9e5fc200c2d7bef70f4546026";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-uKMuJnCINAJWdEapZeTaenBptJN+rIrh5Ml+9J7Gr+4=";
    };
    "bin/hey" = builtins.readFile ./hlissner-hey-089f1a9da9018df9e5fc200c2d7bef70f4546026/bin/hey;
    "config/zsh/completions/_hey" = builtins.readFile ./hlissner-hey-089f1a9da9018df9e5fc200c2d7bef70f4546026/config/zsh/completions/_hey;
    date = "2022-09-19";
  };
  hug = {
    pname = "hug";
    version = "2.6.1";
    src = fetchurl {
      url = "https://pypi.io/packages/source/h/hug/hug-2.6.1.tar.gz";
      sha256 = "sha256-sO2s4qy2GIc3ecnObs+RZdtU/vlcIiYvVwD83Z/rrsk=";
    };
  };
  kitty-bortflower-icons = {
    pname = "kitty-bortflower-icons";
    version = "269c0f0bd1c792cebc7821f299ce9250ed9bcd67";
    src = fetchgit {
      url = "https://github.com/DinkDonk/kitty-icon.git";
      rev = "269c0f0bd1c792cebc7821f299ce9250ed9bcd67";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-Vy+iLGnysrJMSLfkaYq15pb/wG4kIbfsXRrPgSc3OFs=";
    };
    date = "2022-08-01";
  };
  orgparse = {
    pname = "orgparse";
    version = "0.3.1";
    src = fetchurl {
      url = "https://pypi.io/packages/source/o/orgparse/orgparse-0.3.1.tar.gz";
      sha256 = "sha256-hg5vu5pnt0K6p5LmD4zBhSLpeJwGXSaCHAIoXV/BBK8=";
    };
  };
  promnesia = {
    pname = "promnesia";
    version = "346b2e08e04604adffb17ae51244cd1b1ec9015f";
    src = fetchFromGitHub ({
      owner = "karlicoss";
      repo = "promnesia";
      rev = "346b2e08e04604adffb17ae51244cd1b1ec9015f";
      fetchSubmodules = false;
      sha256 = "sha256-GcLPJZQaHqPUGvcZMNlofwqTizh5/PywA4vY9N3Ih7s=";
    });
    date = "2022-06-08";
  };
  roots-trellis-cli = {
    pname = "roots-trellis-cli";
    version = "v1.7.0";
    src = fetchFromGitHub ({
      owner = "roots";
      repo = "trellis-cli";
      rev = "v1.7.0";
      fetchSubmodules = false;
      sha256 = "sha256-/YUoGPVTbmegr8cmjhtHnG1jiwIjen91sUWjgK3T8GQ=";
    });
  };
  ssh-cmc = {
    pname = "ssh-cmc";
    version = "d5085c30d41ea1db96b1c119338fc6501fd09900";
    src = fetchgit {
      url = "https://github.com/TimidRobot/cmc";
      rev = "d5085c30d41ea1db96b1c119338fc6501fd09900";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-onESIhNu+uNPXFA27Hu041Rhk+epXWfTjIdtN1U8Jpg=";
    };
    cmc = builtins.readFile ./ssh-cmc-d5085c30d41ea1db96b1c119338fc6501fd09900/cmc;
    date = "2022-09-30";
  };
}
