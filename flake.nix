{
  description = "MDL / DDL-LTLf Python toolkit";

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
      mdlPythonPackagesFor = pkgs:
        let
          # Valentine currently declares support below Python 3.15.
          python = pkgs.python312;
          py = python.pkgs;
          potNoCheck = py.pot.overridePythonAttrs (_old: {
            doCheck = false;
          });
          valentine = py.buildPythonPackage rec {
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

            # nixpkgs currently has slightly older compatible builds than
            # Valentine declares for these two packages.
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

            # mdl align only needs the BDI schema-matching adapter. The default
            # package metadata pulls chatbot/value-matching extras that are not
            # available in nixpkgs and are unrelated to the matcher backend.
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
              valentine
            ];

            doCheck = false;
            pythonImportsCheck = [
              "bdikit.schema_matching.valentine"
            ];
          };
        in
        {
          inherit
            python
            valentine
            bdikit
            ;
        };
      mdlFor = pkgs: { withSolver ? true, withAligners ? true }:
        let
          mdlPythonPackages = mdlPythonPackagesFor pkgs;
          python = mdlPythonPackages.python;
          antlrRuntime = with python.pkgs; [
            antlr4-python3-runtime
          ];
          solverDependencies = lib.optionals withSolver (with python.pkgs; [
            z3-solver
          ]);
          alignerDependencies =
            lib.optionals withAligners
              (with python.pkgs; [
                pandas
              ])
            ++ lib.optionals withAligners [
              mdlPythonPackages.valentine
              mdlPythonPackages.bdikit
            ];
          importChecks = [
            "antlr4"
            "mdl"
          ]
          ++ lib.optionals withSolver [
            "z3"
          ]
          ++ lib.optionals withAligners [
            "valentine"
            "bdikit.schema_matching.valentine"
          ];
        in
        python.pkgs.buildPythonPackage {
          pname = "mprokazin-mdl";
          version = "0.1.1";
          src = ./mdl;
          pyproject = true;

          build-system = with python.pkgs; [
            setuptools
            wheel
          ];

          dependencies = antlrRuntime ++ solverDependencies ++ alignerDependencies;

          pythonRelaxDeps =
            lib.optionals withAligners [ "pandas" ]
            ++ lib.optionals withSolver [ "z3-solver" ];

          pythonRemoveDeps = lib.optionals withSolver [ "z3-solver" ];

          pythonImportsCheck = importChecks;

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
          mdl = mdlFor pkgs { };
          mdlLanguage = mdlFor pkgs {
            withSolver = false;
            withAligners = false;
          };
        in
        {
          mdl = mdl;
          mdl-language = mdlLanguage;
          default = mdl;
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.uv
              pkgs.z3
              pkgs.basedpyright
              pkgs.ruff
              pkgs.antlr4
              pkgs.jdk
              pkgs.tree-sitter
              pkgs.nodejs
              pkgs.neovim
              pkgs.gcc
              pkgs.vsce
            ];

            UV_PYTHON_DOWNLOADS = "never";
            UV_PYTHON = "${pkgs.python312}/bin/python3";
          };
        });
    };
}
