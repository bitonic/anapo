{ ghcjs ? true
, ghcVersion ? "ghc822"
, ghcjsVersion ? "ghcjsHEAD"
}:
let
  pkgs = import (import ./nixpkgs.nix) {};
  jsaddleHEAD = import (import ./jsaddle.nix);
  haskellPackagesBase = if ghcjs
    then pkgs.haskell.packages.${ghcjsVersion}
    else pkgs.haskell.packages.${ghcVersion};
  haskellPackages = haskellPackagesBase.override {
    overrides = self: super: {
      jsaddle = (jsaddleHEAD self).jsaddle;
      # the jsaddle-warp tests fail because phantomjs is not there
      jsaddle-warp = pkgs.haskell.lib.dontCheck (jsaddleHEAD self).jsaddle-warp;
    };
  };
  anapo = haskellPackages.callPackage ./anapo/anapo.nix {};
  anapo-test-app = haskellPackages.callPackage ./anapo-test-app/anapo-test-app.nix {
    inherit anapo;
  };
in { inherit anapo anapo-test-app; }
