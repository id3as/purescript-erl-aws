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
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "13ace3addf14dd9e93af9132e4799b7badfbe99e";
      sha256 = "1gva113kyygjhn9i92vg6cyj15vhyfhq7haq51cvp4xdz4j0q4xn";
    }) {};

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

  pose = nixpkgs.nodePackages.purty.override {
      name = "prettier-plugin-purescript";
      packageName = "prettier-plugin-purescript";
      version = "1.11.1";
      src = builtins.fetchurl {
        url = "https://registry.npmjs.org/@rowtype-yoga/prettier-plugin-purescript/-/prettier-plugin-purescript-1.11.1.tgz";
      };
      meta = {
        description = "Hacked in Purescript Prettier Plugin";
        license = "MIT";
      };
    };

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
    purerl.purerl-0-0-14

    # More formatting
    nodePackages.prettier
    pose

    # Kind of essential
    awscli2
  ];
  shellHook =
    '' 
      export PRETTIER_PURESCRIPT=${pose.out}/lib/node_modules/prettier-plugin-purescript/index.js
    '';
}
