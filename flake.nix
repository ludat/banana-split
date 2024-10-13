{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to match Stackage LTS version from stack.yaml resolver
        hPkgs = pkgs.haskell.packages.ghc966;

        bananasplitDeps = with pkgs; [
          zlib
          blas
          lapack
          glpk
          postgresql
          nodePackages.pnpm
          nodejs
          reshape
        ];

        myDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          hPkgs.stylish-haskell
          stack-wrapped
        ] ++ bananasplitDeps;

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          version = pkgs.stack.version;
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        elm-ui = pkgs.stdenv.mkDerivation {
          name = "banana-split-elm";
          __noChroot = true;
          src = with pkgs.lib.fileset; toSource {
            root = ./ui;
            fileset = unions [
              ./ui/package.json
              ./ui/pnpm-lock.yaml
              ./ui/.npmrc

              ./ui/src

              ./ui/elm.json
              ./ui/elm-land.json
            ];
          };
          nativeBuildInputs = with pkgs; [ nodejs nodePackages.pnpm cacert ];

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
          buildInputs = with pkgs; [ reshape ];
          postBuild = ''
            mkdir -p $out/opt/banana-split/migrations
            mkdir -p $out/bin/

            cp -v ${pkgs.reshape}/bin/reshape $out/bin/
            cp -vr . $out/opt/banana-split/migrations
          '';
        };

        banana-split = pkgs.haskell.lib.buildStackProject {
          name = "banana-split";
          src = with pkgs.lib.fileset; toSource {
            root = ./.;
            fileset = unions [
              ./app
              ./package.yaml
              ./src
              ./stack.yaml
              ./stack.yaml.lock
              # ./tests
            ];
          };
          nativeBuildInputs = with pkgs; [git];
          doCheck = false;
          stack = stack-wrapped;
          buildInputs = bananasplitDeps;
        };
      in {
        packages = {
          inherit banana-split;
          elm-ui = elm-ui;
          migrations = migrations;
          docker = pkgs.dockerTools.buildImage {
            name = "banana-split";
            tag = "latest";
            created = "now";
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = with pkgs; [
                banana-split
                elm-ui
                migrations
                dockerTools.binSh
                iana-etc
                cacert
              ];
            };
            config = {
              Cmd = ["/bin/banana-split"];
              WorkingDir = "/opt/banana-split";
            };
          };
          default = banana-split;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}
