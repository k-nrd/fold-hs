{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.fold-nix.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.fold-nix.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
