{ pkgs
, lib
, stdenv
, gitignoreSource
, leiningen
}:

stdenv.mkDerivation {
  name = "conexp-clj-dependencies";
  nativeBuildInputs = [ leiningen ];
  src = gitignoreSource ../..;

  buildPhase = ''
    runHook preBuild

    export HOME=$PWD
    export LEIN_HOME=$HOME/.lein
    mkdir -p $LEIN_HOME

    _JAVA_OPTIONS="-Duser.home=$out" lein with-profile uberjar,dev,debug deps
    # hack to get repl dependencies into the repo
    echo | _JAVA_OPTIONS="-Duser.home=$out" lein repl
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    find $out/.m2 -type f -regex '.+\(\.lastUpdated\|resolver-status\.properties\|_remote\.repositories\|maven-metadata-local\.xml\)' -delete

    runHook postInstall
  '';

  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = "sha256-o+DQDDs3Ch+cGuMoFGB6xGFwJKBM/946X6bFzlBiaVo=";
}
