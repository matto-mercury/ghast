{ mkDerivation, aeson, aeson-casing, attoparsec, base
, base64-bytestring, bytestring, case-insensitive, exceptions
, hspec, hspec-attoparsec, hspec-discover, http-conduit, http-types
, lib, mtl, optparse-generic, shelly, text, uri-encode
}:
mkDerivation {
  pname = "ghast";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing attoparsec base base64-bytestring bytestring
    case-insensitive exceptions http-conduit mtl optparse-generic
    shelly text uri-encode
  ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive http-conduit text
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive exceptions hspec
    hspec-attoparsec http-conduit http-types text
  ];
  testToolDepends = [ hspec-discover ];
  description = "A CLI tool for checking Github Actions status on your current branch";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
