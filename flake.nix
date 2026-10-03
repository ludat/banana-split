{
  description = "my project description";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    MIP = {
      url = "github:msakai/haskell-MIP";
      flake = false;
    };
    conferer = {
      url = "github:ludat/conferer";
      flake = false;
    };
    # Los de nixpkgs son de 2023 (solo traces, otro layout de módulos) o están
    # marcados como broken. El pin tiene que coincidir con el
    # `source-repository-package` de `cabal.project`.
    hs-opentelemetry = {
      url = "github:iand675/hs-opentelemetry/5a267e690941bc3817b1de8dbd496cb35bb4788c";
      flake = false;
    };
    # `hs-opentelemetry-api` necesita >= 0.4.1.1 (ver el comentario del override
    # más abajo), y esa versión es más nueva que el snapshot de
    # `all-cabal-hashes` que trae nuestro nixpkgs. Pedirla como
    # `source = "0.4.1.1"` falla con
    # "thread-utils-context.cabal: Not found in archive", así que va del repo.
    thread-utils = {
      url = "github:iand675/thread-utils/4a981ca2a0dd67cb6dce81cb8465612cfb3b1ced";
      flake = false;
    };
  };

  outputs =
    inputs@{ flake-parts, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskellPackages;
            projectRoot =
              with lib.fileset;
              toSource {
                root = ./.;
                fileset = unions [
                  ./package.yaml
                  ./src
                  ./cabal.project
                  ./banana-split.cabal
                  ./test
                ];
              };

            settings = {
              banana-split.check = false;
            };

            # Packages to add on top of `basePackages`, e.g. from Hackage
            packages = {
              MIP.source = inputs.MIP + /MIP;
              conferer.source = inputs.conferer + /packages/conferer;
              conferer-warp.source = inputs.conferer + /packages/warp;
              jose.source = "0.12";

              # Los mismos subdirectorios que el `source-repository-package` de
              # `cabal.project`, más `exporters/in-memory`: ese es dependencia
              # del test-suite del sdk, y haskell-flake corre los checks de las
              # dependencias mientras cabal no compila los tests de las suyas.
              # Si se agrega un subdirectorio en uno, va también en el otro.
              hs-opentelemetry-api.source = inputs.hs-opentelemetry + /api;
              hs-opentelemetry-api-types.source = inputs.hs-opentelemetry + /api-types;
              hs-opentelemetry-semantic-conventions.source = inputs.hs-opentelemetry + /semantic-conventions;
              hs-opentelemetry-otlp.source = inputs.hs-opentelemetry + /otlp;
              hs-opentelemetry-sdk.source = inputs.hs-opentelemetry + /sdk;
              hs-opentelemetry-exporter-handle.source = inputs.hs-opentelemetry + /exporters/handle;
              hs-opentelemetry-exporter-otlp.source = inputs.hs-opentelemetry + /exporters/otlp;
              hs-opentelemetry-exporter-in-memory.source = inputs.hs-opentelemetry + /exporters/in-memory;
              hs-opentelemetry-propagator-b3.source = inputs.hs-opentelemetry + /propagators/b3;
              hs-opentelemetry-propagator-datadog.source = inputs.hs-opentelemetry + /propagators/datadog;
              hs-opentelemetry-propagator-jaeger.source = inputs.hs-opentelemetry + /propagators/jaeger;
              hs-opentelemetry-propagator-w3c.source = inputs.hs-opentelemetry + /propagators/w3c;
              hs-opentelemetry-propagator-xray.source = inputs.hs-opentelemetry + /propagators/xray;

              # `hs-opentelemetry-api` usa la API de `Ref` de este paquete, que
              # apareció en 0.4.1.1, pero declara `>= 0.3 && < 0.5`. Con ese
              # bound el solver puede elegir 0.3.0.4 y la compilación de otel
              # falla con "Variable not in scope: ensureRef" — un error que
              # parece de otel y es de resolución.
              #
              # Acá va del repo y no como `source = "0.4.1.1"` porque esa
              # versión es más nueva que el snapshot de `all-cabal-hashes` de
              # nuestro nixpkgs. Del lado de cabal sale de Hackage, forzada por
              # el `constraints` de `cabal.project`; es la misma versión.
              thread-utils-context.source = inputs.thread-utils + /thread-utils-context;
            };

            # my-haskell-package development shell configuration
            devShell = {
              hlsCheck.enable = true;
              tools =
                hp:
                with hp;
                with pkgs;
                {
                  inherit
                    kubernetes-helm
                    kind
                    cabal-gild
                    process-compose
                    zlib
                    blas
                    lapack
                    cbc
                    tesseract
                    fourmolu
                    glpk
                    libpq
                    mailpit
                    ;
                };
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          devShells.default = pkgs.mkShell {
            name = "my-haskell-package custom development shell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              self'.packages.elm-ui
              self'.packages.migrations
            ];
          };
          packages = {
            default = pkgs.haskell.lib.justStaticExecutables self'.packages.banana-split;

            elm-ui = pkgs.stdenv.mkDerivation {
              name = "banana-split-elm";
              __noChroot = true;
              src =
                with pkgs.lib.fileset;
                toSource {
                  root = ./ui;
                  fileset = unions [
                    ./ui/package.json
                    ./ui/pnpm-lock.yaml
                    ./ui/.npmrc

                    ./ui/static
                    ./ui/src
                    ./ui/generated-src
                    ./ui/tests

                    ./ui/elm.json
                    ./ui/elm-land.json
                    ./ui/elm.sideload.json
                  ];
                };
              nativeBuildInputs = with pkgs; [
                nodePackages.pnpm
                nodejs
                git
                cacert
              ];

              buildPhase = ''
                export HOME=$PWD
                export ELM_HOME=$HOME/.elm-home
                pnpm install --reporter=append-only --frozen-lockfile
                pnpm run build
              '';
              installPhase = ''
                mkdir -p $out/opt/banana-split
                mv -v dist/ $out/opt/banana-split/public
              '';
            };

            migrations = pkgs.stdenv.mkDerivation {
              name = "banana-split-migrations";
              src = ./migrations;
              buildInputs = with pkgs; [ pgroll ];
              postBuild = ''
                mkdir -p $out/opt/banana-split/migrations
                mkdir -p $out/bin/

                cp -v ${pkgs.pgroll}/bin/pgroll $out/bin/
                cp -vr . $out/opt/banana-split/migrations
              '';
            };

            docker = pkgs.dockerTools.buildImage {
              name = "banana-split";
              tag = "latest";
              created = "now";
              copyToRoot = pkgs.buildEnv {
                name = "image-root";
                paths = with pkgs; [
                  self'.packages.default
                  self'.packages.elm-ui
                  self'.packages.migrations
                  dockerTools.binSh
                  iana-etc
                  cacert
                  busybox
                  cbc
                  # tesseract
                  (writeShellScriptBin "entrypoint" ''
                    set -euo pipefail
                    find /opt/banana-split -exec touch -d "@${toString inputs.self.lastModified}" {} +;
                    exec "$@";
                  '')

                ];
              };
              config = {
                Cmd = [
                  "banana-split"
                  "server"
                ];
                Entrypoint = [ "entrypoint" ];
                WorkingDir = "/opt/banana-split";
              };
            };
          };
        };
    };
}
