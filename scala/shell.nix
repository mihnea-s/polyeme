{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    scala
    sbt
    metals
    graalvm17-ce
  ];
}
