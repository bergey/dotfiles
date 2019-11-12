pkgs :
{
  allowUnfree = true;
  # allowBroken = true;
  packageOverrides = self: with self; rec {

    arduinoEnv = myEnvFun {
        name = "arduino";
        buildInputs = [
            arduino_core
            avrgcclibc
            avrdude
            ino
            stdenv
        ];
    };

    scalaEnv = myEnvFun {
        name = "scala";
        buildInputs = [
            sbt
            scala
        ];
    };

    clojureEnv = myEnvFun {
        name = "clojure";
        buildInputs = [
            leiningen
            clojure
        ];
    };

    };
}
