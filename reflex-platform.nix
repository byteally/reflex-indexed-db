let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
     reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "850a9613ba6bc386d1a8b8896c6f65cc9c756a69";
      sha256 = "0w7famcbpxr14z9aah00j2sgf6qb99ndf6xdvj62pk1qn0i2h3i3";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
