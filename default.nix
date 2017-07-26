let
  _pkgs = import <nixpkgs> {};
in
{ pkgs ? import (_pkgs.fetchFromGitHub
  { owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "0afb6d789c";
    sha256 = "147vhzrnwcy0v77kgbap31698qbda8rn09n5fnjp740svmkjpaiz";
  }) {}
}:
with pkgs;

stdenv.mkDerivation {
  name = "none";
  buildInputs = [
    elmPackages.elm
  ];
}
