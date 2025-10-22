{
  description = "anki dictionary feed";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          dictionary =
            final.haskell-nix.project' {
              src = ./.;
              # src = builtins.path {
              #   path = ./.;
              #   name = "my-src";
              #   filter = path: type:
              #   !(
              #     builtins.match
              #     ".*/(node_modules|.git|result|dist|target|build|.direnv)" path != null
              #     );
              #   };
              compiler-nix-name = "ghc928";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = "latest";
                # hlint = {};
                haskell-language-server = "latest";
                hoogle = "latest";
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.dictionary.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."dictionary:exe:dictionary";
      apps.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/dictionary";
      };
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    bash-prompt = "\[nix-develop\]$ ";
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org/"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
    allow-import-from-derivation = "true";
  };
}
