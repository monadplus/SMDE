{ compiler ? "ghc881" }:
let

  pkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "890440b0273caab258b38c0873c3fe758318bb4f";
    ref = "master";
  }) {};

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          bytestring
          cassava
          mwc-random
          string-conv
        ]);

in

pkgs.stdenv.mkDerivation {
  name = "ghc-env";
  buildInputs = [ ghc pkgs.cabal-install ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
