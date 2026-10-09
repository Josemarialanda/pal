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

          # Build and start the interactive PAL REPL.
          #   pal-repl                              # empty REPL
          #   pal-repl examples/programs/stlc.pal   # load files first, then REPL
          # Files are run in order in one context before the prompt appears
          # (the same as `pal -i FILE...`). Relative paths are resolved from the
          # directory you run it in, not from the project root.
          pal-repl = pkgs.writeShellApplication {
            name = "pal-repl";
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
              if [ "$#" -eq 0 ]; then
                exec "$pal_bin"
              else
                exec "$pal_bin" --interactive "$@"
              fi
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
              pal-repl
              run-examples
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
