(import ./reflex-platform.nix).project ({ pkgs, ... }: {
  packages = {
    reflex-indexed-db = ./.;
  };
  overrides = self : super : {
    
  };

  shells = {
    ghc   = ["reflex-indexed-db" ];
    ghcjs = ["reflex-indexed-db" ];
  };
})