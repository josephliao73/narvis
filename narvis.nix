{ pkgs ? import <nixpkgs> {} }:

let
  narvis = pkgs.haskellPackages.callCabal2nix "narvis" ./. { };
in
pkgs.dockerTools.buildImage {
  name = "narvis";
  tag  = "latest";

  contents = [ narvis pkgs.cacert ];

  config = {
    Env = [
      "PATH=/bin"
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
    ];
    Cmd = [ "narvis" ];
  };
}

