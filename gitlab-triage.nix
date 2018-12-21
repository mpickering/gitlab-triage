{ mkDerivation, aeson, base, bytestring, containers, directory
, exceptions, http-client-tls, http-types, mtl, process, servant
, servant-client, stdenv, text, time, connection, data-default, reflex, reflex-brick, brick, microlens, vector, vty
}:
mkDerivation {
  pname = "gitlab-triage";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory exceptions mtl process
    servant servant-client text time
  ];
  executableHaskellDepends = [
    base http-client-tls http-types servant servant-client text connection data-default
    reflex reflex-brick brick microlens vector vty
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
