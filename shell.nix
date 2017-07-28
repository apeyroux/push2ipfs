{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "push2ipfs";
  buildInputs = [ haskellPackages.stack haskellPackages.cabal-install haskell.compiler.ghc802 zlib ];
  shellHook = ''
    export LD_LIBRARY_PATH="${zlib}/lib:$LD_LIBRARY_PATH"
  '';
}
