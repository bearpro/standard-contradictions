{
  description = "MDL / DDL-LTLf Python toolkit";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { nixpkgs, ... }: {
    devShells = nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
    ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        default = pkgs.mkShell {
          packages = [
            pkgs.python312
            pkgs.uv
            pkgs.ruff
            pkgs.basedpyright
            pkgs.z3
            pkgs.gcc
            pkgs.antlr4
            pkgs.jdk
            pkgs.nodejs
            pkgs.tree-sitter
            pkgs.vsce
          ];

          UV_PYTHON_DOWNLOADS = "never";
          UV_PYTHON = "${pkgs.python312}/bin/python3";
          Z3_LIBRARY_PATH = "${pkgs.z3}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.z3
            pkgs.stdenv.cc.cc.lib
          ];
        };
      });
  };
}
