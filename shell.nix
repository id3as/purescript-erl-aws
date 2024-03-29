let
  pinnedNixHash = "02336c5c5f719cd6bd4cfc5a091a1ccee6f06b1d";

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "${pinnedNixHash}";
    };

  erlangReleases =
    builtins.fetchGit {
      name = "nixpkgs-nixerl";
      url = "https://github.com/id3as/nixpkgs-nixerl.git";
      rev = "7206f820c54e4414f1a9b7b48e10f736138bb625";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "9812fbd38fb4e259af94eaeca2b068332e4e648c";
    };

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      ## Temporarily on Fabrizio's fork to get spago-next
      owner = "f-f";
      repo = "easy-purescript-nix";
      rev = "2e62b746859e396e541bdd63dbd10b2f231027d4";
      sha256 = "sha256-qQpWKE40wKkxb4y2+z0n4lF/OFrCsEU3Gwiugl3H+xc=";
    }) { pkgs = nixpkgs; };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
      ];
    };

  erlang = nixpkgs.nixerl.erlang-24-1-3.overrideScope' (self: super: {
    erlang = super.erlang.override {
      wxSupport = false;
    };
  });

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

mkShell {
  buildInputs = with pkgs; [

    erlang.erlang
    erlang.rebar3
    erlang.erlang-ls

    # Purescript
    easy-ps.purs-0_15_9-2
    easy-ps.spago-next
    easy-ps.psa
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    purerl.purerl-0-0-19

    # Kind of essential
    awscli2
  ];
}
