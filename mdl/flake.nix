{
  description = "MDL command-line toolkit";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      lib = nixpkgs.lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      pkgsFor = system: import nixpkgs { inherit system; };

      source = lib.cleanSourceWith {
        src = ./.;
        filter = path: _type:
          let
            name = baseNameOf path;
          in
            !(lib.hasSuffix ".egg-info" name
              || name == ".venv"
              || name == "__pycache__"
              || name == ".pytest_cache"
              || name == ".ruff_cache");
      };

      mdlStdlibFor = pkgs:
        pkgs.runCommand "mdl-stdlib-0.1.1" { } ''
          mkdir -p "$out"
          cp -R "${source}/src/mdl/stdlib/." "$out/"
        '';

      mdlFor = pkgs:
        let
          mdlStdlib = mdlStdlibFor pkgs;
          python = pkgs.python312;
          py = python.pkgs;
          potNoCheck = py.pot.overridePythonAttrs (_old: {
            doCheck = false;
          });
          valentineForBdikit = py.buildPythonPackage rec {
            pname = "valentine";
            version = "0.5.0";
            pyproject = true;

            src = py.fetchPypi {
              inherit pname version;
              hash = "sha256-N+q+pYLg467lC85k7idNDy9wloK1Bj1yeqSy9tjuBFA=";
            };

            build-system = with py; [
              setuptools
            ];

            dependencies = (with py; [
              anytree
              chardet
              jellyfish
              networkx
              nltk
              numpy
              pandas
              pulp
              python-dateutil
              scipy
            ]) ++ [
              potNoCheck
            ];

            pythonRelaxDeps = [
              "chardet"
              "nltk"
            ];

            doCheck = false;
            pythonImportsCheck = [
              "valentine"
              "valentine.algorithms"
            ];
          };
          bdikit = py.buildPythonPackage rec {
            pname = "bdi-kit";
            version = "0.10.0";
            pyproject = true;

            src = pkgs.fetchurl {
              url = "https://files.pythonhosted.org/packages/ba/e8/a51c8fdb3d65ad55af311209f24b291c6e051ed4831060259523a6197252/bdi_kit-0.10.0.tar.gz";
              hash = "sha256-c2dPKI8LDM8u4rqkmz8WnAAlaimvVGMszlQ4yVTZmC4=";
            };

            build-system = with py; [
              setuptools
              wheel
            ];

            postPatch = ''
              printf '%s\n' \
                numpy \
                pandas \
                'valentine>=0.5.0' \
                > requirements.txt

              printf '%s\n' '__version__ = "0.10.0"' > bdikit/__init__.py
            '';

            dependencies = with py; [
              numpy
              pandas
              valentineForBdikit
            ];

            doCheck = false;
            pythonImportsCheck = [
              "bdikit.schema_matching.valentine"
            ];
          };
        in
        python.pkgs.buildPythonPackage {
          pname = "mprokazin-mdl";
          version = "0.1.1";
          src = source;
          pyproject = true;

          build-system = with python.pkgs; [
            setuptools
            wheel
          ];

          dependencies = with python.pkgs; [
            z3-solver
            pandas
            bdikit
          ];

          pythonRelaxDeps = [
            "pandas"
            "z3-solver"
          ];
          pythonRemoveDeps = [
            "z3-solver"
          ];

          makeWrapperArgs = [
            "--set"
            "MDL_STDLIB_PATH"
            "${mdlStdlib}"
          ];

          pythonImportsCheck = [
            "mdl"
            "z3"
            "bdikit.schema_matching.valentine"
          ];

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
          mdlStdlib = mdlStdlibFor pkgs;
        in
        {
          inherit mdl;
          mdl-stdlib = mdlStdlib;
          default = mdl;
        });

      apps = forAllSystems (system: {
        mdl = {
          type = "app";
          program = "${self.packages.${system}.mdl}/bin/mdl";
          meta.description = "Run the MDL command-line toolkit";
        };
        default = self.apps.${system}.mdl;
      });
    };
}
