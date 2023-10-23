{ pkgs}:

# unreleased version actually works
{ mkDerivation, base, bytestring, cmdargs, deepseq, dlist, lens
, parallel-io, regex-posix, system-fileio, system-filepath, text
, unix
}:

mkDerivation {
  pname = "sizes";
  version = "2.3.2-git";
  src = pkgs.fetchgit {
    url = "https://github.com/jwiegley/sizes.git";
    sha256 = "0ma8kxw1sh3bg2rqgq7i6pbjg5frjvyyjshma8q98r8sqa579yn2";
    rev = "1658de1c70d505f2e5d9d736975a8bfae77799f1";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cmdargs deepseq dlist lens parallel-io regex-posix
    system-fileio system-filepath text unix
  ];
  description = "Recursively show space (size and i-nodes) used in subdirectories";
  license = pkgs.lib.licenses.bsd3;
  hydraPlatforms = pkgs.lib.platforms.none;
  broken = false;
}
