{
  description = "Minimal dev shell for .NET 10, Python, uv, LSP servers, ANTLR, and Z3";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { nixpkgs, ... }:
    let
      lib = nixpkgs.lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      pkgsFor = system: import nixpkgs { inherit system; };
      mdlFor = pkgs:
        let
          python = pkgs.python315;
        in
        python.pkgs.buildPythonPackage {
          pname = "mdl-ddl-ltlf";
          version = "0.1.0";
          src = ./mdl-v3;
          pyproject = true;

          build-system = with python.pkgs; [
            setuptools
            wheel
          ];

          pythonImportsCheck = [ "mdl" ];

          meta = {
            description = "MDL / DDL-LTLf modelling language toolkit";
            license = lib.licenses.mit;
            mainProgram = "mdl";
          };
        };
    in
    {
      formatter = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.nixpkgs-fmt);

      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          mdl = mdlFor pkgs;
        in
        {
          inherit mdl;
          default = mdl;
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          dotnet = pkgs.dotnet-sdk_10;
          python = pkgs.python315;
          jre = pkgs.jre_headless;
          mdl = mdlFor pkgs;
        in
        {
          default = pkgs.mkShell {
            packages = [
              mdl
              dotnet
              python
              pkgs.uv
              pkgs.antlr4
              jre
              pkgs.z3
              pkgs.fsautocomplete
              pkgs.basedpyright
              pkgs.tree-sitter
              pkgs.nodejs
              pkgs.neovim
              pkgs.gcc
              pkgs.vsce
            ];

            DOTNET_ROOT = "${dotnet}";
            JAVA_HOME = "${jre}";
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.z3 ];
            UV_PYTHON_DOWNLOADS = "never";
          };
        });
    };
}
