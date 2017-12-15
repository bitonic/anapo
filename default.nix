haskellPackages: {
  anapo = haskellPackages.callPackage ./anapo/anapo.nix {};
  anapo-test-app = haskellPackages.callPackage ./anapo-test-app/anapo-test-app.nix {};
}
