{haskellPackages, bundleVdom ? true}: {
  anapo = haskellPackages.callPackage ./anapo/anapo.nix {inherit bundleVdom;};
  anapo-test-app = haskellPackages.callPackage ./anapo-test-app/anapo-test-app.nix {};
} // (if haskellPackages.ghc.isGhcjs or false then {} else {
  anapo-blaze = haskellPackages.callPackage ./anapo-blaze/anapo-blaze.nix {};
})

