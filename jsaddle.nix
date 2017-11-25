let pkgs = import <nixpkgs> {};
in pkgs.fetchFromGitHub {
 owner = "ghcjs";
 repo = "jsaddle";
 rev = "37051d69103bd6406695324eb7788b1841879716";
 sha256 = "0cchg4fzyaj4mpxnm4lvjxlynaygh9r35zcr0sxvs2lcn5gk5hkr";
}
