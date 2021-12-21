{ pkgs
, lib
, stdenv
, makeWrapper
, strip-nondeterminism
, gitignoreSource
, leiningen
, jdk
}:

let dependencies = pkgs.callPackage ./dependencies.nix { inherit gitignoreSource leiningen; };
in
stdenv.mkDerivation rec {
  pname = "conexp-clj";
  version = "2.3.0-SNAPSHOT";
  src = gitignoreSource ../..;

  buildInputs = [ makeWrapper ];
  nativeBuildInputs = [ leiningen ];

  preBuild = ''
    mkdir -p $out
    mkdir -p $out/maven
    cp -R ${dependencies}/.m2 $out/maven
    chmod -R +w $out/maven
  '';

  buildPhase = ''
    runHook preBuild

    export HOME=$PWD
    export LEIN_HOME=$HOME/.lein
    mkdir -p $LEIN_HOME

    _JAVA_OPTIONS="-Duser.home=$out/maven" lein -o uberjar

    runHook postBuild
  '';

  postFixup = ''
    ${strip-nondeterminism}/bin/strip-nondeterminism $out/${pname}-${version}-standalone.jar
  '';

  installPhase = ''
    cp builds/uberjar/${pname}-${version}-standalone.jar $out
    makeWrapper ${jdk}/bin/java $out/bin/${pname} --add-flags "-jar $out/${pname}-${version}-standalone.jar"
    makeWrapper ${leiningen}/bin/lein $out/bin/lein --add-flags "-o" --set "_JAVA_OPTIONS" "-Duser.home=$out/maven"
  '';
}
