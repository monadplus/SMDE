let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [ mwc-random cassava bytestring ]))
  ];
}
