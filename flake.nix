{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [
            haskell-nix.overlay
            (self: super: {
              gen =
                self.haskell-nix.project'
                  {
                    src = ./.;

                    compiler-nix-name = "ghc8107";

                    # development shell
                    shell = {
                      # haskell tools
                      tools = {
                        cabal = { };
                        haskell-language-server = { };
                        hindent = { };
                      };

                      # non-haskell tools
                      buildInputs = with pkgs; [
                        # for `digest`
                        zlib-ng

                        stylish-haskell
                        nixpkgs-fmt
                      ] ++ lib.attrsets.attrValues scripts;
                    };
                  };
            })
          ];

          scripts = rec {
            gen = pkgs.writeShellScriptBin "gen" ''
              runghc ./app/Main.hs $@
            '';

            watch-content = pkgs.writeShellScriptBin "watch-content" ''
              ${gen}/bin/gen clean
              ${gen}/bin/gen watch
            '';

            watch-all = pkgs.writeShellScriptBin "watch-all" ''
              ${pkgs.watchexec}/bin/watchexec -w app -w content -r ${watch-content}/bin/watch-content
            '';
          };

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskell-nix) config;
          };
          inherit (pkgs) lib;

          flake = pkgs.gen.flake { };
        in
        flake // {
          defaultPackage = flake.packages."gen:exe:gen";

          apps = rec {
            gen = {
              type = "app";
              program = "${flake.packages."gen:exe:gen"}/bin/gen";
            };

            default = gen;
          };
        }
      );
}
