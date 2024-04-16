inputs: system:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
    src = pkgs.fetchFromGitHub {
      owner = "dcspark";
      repo = "go-ethereum";
      rev = "352d900fea2cd96b5dc1ee08527e39db994f98a5";
      sha256 = "sha256-emNQSq4Uhi8GM5vTkP8vqPsQbkjnnla/IYFQrFxQbGM=";
    };
in
rec {
  mina-geth-helper = pkgs.rustPlatform.buildRustPackage {
    pname = "mina-geth-helper";
    version = "dev";
    src = "${src}/mina";
    cargoHash = "sha256-XcmnYSVkF9kkkYTlckzpVn6+sp0Dx2H5mjKSspJR++8=";
  };
  mina-geth = pkgs.go-ethereum.overrideAttrs (o: {
    inherit src;
    prePatch = ''
      sed -ie 's|''${SRCDIR}/../../mina|${mina-geth-helper}|g' core/vm/mina_contracts.go
    '';
    preBuild = ''
      ln -s ${mina-geth-helper}/lib/libmina.a
    '';
    buildInputs = (o.buildInputs or []) ++ [ mina-geth-helper ];
  });
}
