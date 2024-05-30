inputs: system:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
    src = pkgs.fetchFromGitHub {
      owner = "dcspark";
      repo = "go-ethereum";
      rev = "daee9a8b65f5ba06c36d38bc5a6ed3609a505c9b";
      sha256 = "sha256-x7tY5Veh1fcoh/jMqBThJuQIkMuY2vUl9s/qRPM7soc=";
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
