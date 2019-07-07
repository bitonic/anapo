let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner = "bitonic";
 repo = "jsaddle";
 rev = "fbbd7c1d320dd7eddb965e365744c2405618b4dd";
 sha256 = "1xg4phpyybdkrhg43n82dlcw2j8qpn3qnn4zdxyj284mpvdng5zr";
}
