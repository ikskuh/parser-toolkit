{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = [
    # zig
    pkgs.zig_0_11
  ];
  buildInputs = [ ];
  shellHook = ''
    # put your shell hook here
  '';
}
