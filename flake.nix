{
  description = "PAL is a meta-DSL for defining, exploring, and experimenting with type systems and languages within Haskell.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              pal = hfinal.callCabal2nix "pal" ./. { };
            };
        };
        pal = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.pal;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;

          # Format Haskell sources with ormolu.
          #   format            # format every tracked .hs file
          #   format --check    # fail (without writing) if anything is unformatted
          #   format PATH...    # format only the given files/directories
          # Also available as `nix fmt` and `nix run .#format`.
          format = pkgs.writeShellApplication {
            name = "format";
            runtimeInputs = [ hspkgs.ormolu pkgs.git ];
            text = ''
              mode=inplace
              if [ "''${1:-}" = "--check" ]; then
                mode=check
                shift
              fi
              [ "$#" -gt 0 ] || set -- "$(git rev-parse --show-toplevel)"
              files=()
              for path in "$@"; do
                if [ -d "$path" ]; then
                  # Directories (e.g. the `.` that `nix fmt` passes) expand to their tracked .hs files.
                  mapfile -t -O "''${#files[@]}" files < <(git ls-files --full-name -- "$path/*.hs" | sed "s|^|$(git rev-parse --show-toplevel)/|")
                else
                  files+=("$path")
                fi
              done
              [ "''${#files[@]}" -gt 0 ] || exit 0
              ormolu --mode "$mode" "''${files[@]}"
            '';
          };

          # Build the working tree's `pal` (if needed) and run it with the given arguments.
          #   pal                   # start the REPL
          #   pal FILE.pal          # typecheck a file
          #   pal -i FILE.pal       # run a file, then open a REPL with its context
          #   pal --no-color …      # plain output
          # Relative paths are resolved from the directory you run it in, not
          # from the project root.
          pal = pkgs.writeShellApplication {
            name = "pal";
            runtimeInputs = [ hspkgs.cabal-install pkgs.hpack pkgs.git ];
            text = ''
              root="$(git rev-parse --show-toplevel)"
              # Build from the project root, but keep the caller's working directory
              # so relative file arguments still point where the user meant.
              pal_bin="$(
                cd "$root"
                hpack >/dev/null
                cabal build -v0 exe:pal >&2 || exit 1
                cabal list-bin -v0 exe:pal
              )"
              exec "$pal_bin" "$@"
            '';
          };
          # Build the working tree's `pal-ui` (if needed) and run it: serves the
          # web UI on http://127.0.0.1 and opens it in a browser.
          #   pal-ui                # default port 7337 (any free port if taken)
          #   pal-ui --port N       # a specific port
          #   pal-ui --no-open      # just print the URL
          pal-ui = pkgs.writeShellApplication {
            name = "pal-ui";
            runtimeInputs = [ hspkgs.cabal-install pkgs.hpack pkgs.git ];
            text = ''
              root="$(git rev-parse --show-toplevel)"
              ui_bin="$(
                cd "$root"
                hpack >/dev/null
                cabal build -v0 exe:pal-ui >&2 || exit 1
                cabal list-bin -v0 exe:pal-ui
              )"
              exec "$ui_bin" "$@"
            '';
          };
          # Build `pal` and run the regression tests (tests/run.sh).
          #   run-tests             # run every test
          #   run-tests --accept    # also rewrite tests/fail/*.out from the current output
          run-tests = pkgs.writeShellApplication {
            name = "run-tests";
            runtimeInputs = [ hspkgs.cabal-install pkgs.hpack pkgs.git pkgs.diffutils ];
            text = ''
              cd "$(git rev-parse --show-toplevel)"
              hpack >/dev/null
              cabal build -v0 exe:pal
              PAL="$(cabal list-bin -v0 exe:pal)" exec bash tests/run.sh "$@"
            '';
          };
          # Build and run the PAL examples.
          #   run-examples                    # run every example (results only)
          #   run-examples dsl                # run a group: code | data | dsl | file
          #   run-examples data/pairs         # run a single example
          #   run-examples --trace dsl/maybe  # full Debug trace with context dumps
          #   run-examples --list             # list available examples
          run-examples = pkgs.writeShellApplication {
            name = "run-examples";
            runtimeInputs = [ hspkgs.cabal-install pkgs.hpack pkgs.git ];
            text = ''
              cd "$(git rev-parse --show-toplevel)"
              hpack >/dev/null
              cabal build -v0 exe:pal-examples
              exec cabal run -v0 exe:pal-examples -- "$@"
            '';
          };
        in
        {
          formatter = format;

          # `nix flake check` runs the regression tests against the packaged `pal`.
          checks.default = pkgs.runCommand "pal-tests" { nativeBuildInputs = [ pkgs.pal pkgs.bash pkgs.diffutils ]; } ''
            cd ${./.}
            PAL=pal bash tests/run.sh
            touch $out
          '';
          apps.format = { type = "app"; program = "${format}/bin/format"; };

          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.pal ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              format
              pal
              run-examples
              run-tests
              pal-ui
              pkgs.bashInteractive
              pkgs.hpack
            ];
          };
          defaultPackage = pkgs.pal;
        };
    in
    { inherit overlay; } // 
      inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
