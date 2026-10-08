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
