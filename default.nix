{ ghcjs ? true
, ghcVersion ? "ghc821"
, ghcjsVersion ? "ghcjsHEAD"
}:
let
  pkgs = import <nixpkgs> {};
  jsaddleHEAD = import (import ./jsaddle.nix);
  haskellBase = if ghcjs
    then pkgs.haskell.packages.${ghcjsVersion}
    else pkgs.haskell.packages.${ghcVersion};
  haskell = haskellBase.override {
    overrides = self: super: {
      jsaddle = (jsaddleHEAD self).jsaddle;
    };
  };
  anapo = haskell.callPackage ./anapo.nix {
    haskellPackages = haskell;
  };
in anapo
