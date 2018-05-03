with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "yuten";

  buildInputs = with elmPackages; [
    gnugo
    elm
    elm-compiler
    elm-make
    elm-package
    elm-reactor
    elm-repl
    elm-interface-to-json
    entr
    jq
    nodejs
    yarn
  ];

  shellHook = ''
    export PATH="$PATH:${toString ./node_modules/.bin}"
  '';
}
