let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner   = "NixOS";
 repo    = "nixpkgs";
 rev     = "2dca1a6d6e7c41ae8e46b33902ded2af709c3090";
 sha256  = "1lvrj2k19ns61dxj5q9avv9423kvd6va9zgzag7cbmdpczrms7c3";
}
