{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  shellPaths = builtins.map
    (dir: ./. + "/${dir}/shell.nix")
    (builtins.attrNames (builtins.readDir ./.));

  importedShells = builtins.map
    (path: import path { inherit pkgs; })
    (builtins.filter builtins.pathExists shellPaths);

  mergedShells = builtins.foldl' (a: v: {
    buildInputs = a.buildInputs ++ v.buildInputs;
    nativeBuildInputs = a.nativeBuildInputs ++ v.nativeBuildInputs;
    propagatedBuildInputs = a.propagatedBuildInputs ++ v.propagatedBuildInputs;
    propagatedNativeBuildInputs = a.propagatedNativeBuildInputs ++ v.propagatedNativeBuildInputs;
    shellHook = a.shellHook + "\n" + v.shellHook;
  }) {
    buildInputs = [];
    nativeBuildInputs = [];
    propagatedBuildInputs = [];
    propagatedNativeBuildInputs = [];
    shellHook = "";
  } importedShells;

in pkgs.mkShell mergedShells

