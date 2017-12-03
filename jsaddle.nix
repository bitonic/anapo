let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner = "bitonic";
 repo = "jsaddle";
 rev = "55a9e5af2573a2b0a26b09c2ac87aab3b6de1bc0";
 sha256 = "18xqq657m01zlw0awz671rk56m11a1y3ay9crgfd7920nwc5pfs6";
}
