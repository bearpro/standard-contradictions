{
  description = "Minimal dev shell for .NET 10, Python, uv, LSP servers, ANTLR, and Z3";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          dotnet = pkgs.dotnet-sdk_10;
          python = pkgs.python315;
          jre = pkgs.jre_headless;
        in
        {
          default = pkgs.mkShell {
            packages = [
              dotnet
              python
              pkgs.uv
              pkgs.antlr4
              jre
              pkgs.z3
              pkgs.fsautocomplete
              pkgs.basedpyright
            ];

            DOTNET_ROOT = "${dotnet}";
            JAVA_HOME = "${jre}";
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.z3 ];
            UV_PYTHON_DOWNLOADS = "never";
          };
        });
    };
}
