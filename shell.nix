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
      rev = "0ff4c54219fe60c787334051f3303bdc8ba63e9d";
    };

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      ## Temporarily on Fabrizio's fork to get spago-next
      owner = "f-f";
      repo = "easy-purescript-nix";
      rev = "8ec38d6e474e65b73f54ee6aa725ada171eb884e";
      sha256 = "sha256-Z5vFKw7hWyv4W+rDkTCyfifHXtfa7E9KsvMJFoN5Ko8=";
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
    easy-ps.purs-0_14_5
    easy-ps.spago
    easy-ps.psa
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    purerl.purerl-0-0-14

    # Kind of essential
    awscli2
  ];
}
