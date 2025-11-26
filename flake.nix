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
            basePackages = pkgs.haskell.packages.ghc984;
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
                    glpk
                    libpq
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

                    ./ui/elm.json
                    ./ui/elm-land.json
                  ];
                };
              nativeBuildInputs = with pkgs; [
                nodePackages.pnpm
                nodejs
                git
                cacert
              ];

              buildPhase = ''
                HOME=$PWD
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
