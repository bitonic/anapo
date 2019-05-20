{ ghcjs ? true
, ghcVersion ? "ghc802"
, ghcjsVersion ? "ghcjsHEAD"
}:
let
  pkgs = import (import ./nixpkgs.nix) {};
  jsaddleHEAD = import (import ./jsaddle.nix);
  haskellPackagesBase = if ghcjs
    then pkgs.haskell.packages.${ghcjsVersion}
    else pkgs.haskell.packages.${ghcVersion};
  # use what reflex-platform uses -- we need `synchronously` anyway
  haskellPackages0 = if ghcjs
    then haskellPackagesBase.override {
      overrides = self: super: {
        ghcjs-base = self.callCabal2nix "ghcjs-base" (pkgs.fetchFromGitHub {
          owner = "ghcjs";
          repo = "ghcjs-base";
          rev = "43804668a887903d27caada85693b16673283c57";
          sha256 = "1pqmgkan6xhpvsb64rh2zaxymxk4jg9c3hdxdb2cnn6jpx7jsl44";
        }) {};
      };
    }
    else haskellPackagesBase;
  haskellPackages = haskellPackages0.override {
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
  js-framework-benchmark = haskellPackages.callPackage ./js-framework-benchmark/js-framework-benchmark.nix {
    inherit anapo;
  };
in { inherit anapo anapo-test-app js-framework-benchmark; } // (if ghcjs then {} else {
  anapo-blaze = haskellPackages.callPackage ./anapo-blaze/anapo-blaze.nix { inherit anapo; };
})
