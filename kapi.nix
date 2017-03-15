{ mkDerivation, aeson, attoparsec, base, base-compat, bloodhound
, bson, bytestring, containers, exceptions, hspec, hspec-core
, http-client, http-types, iso8601-time, lifted-base, monad-control
, mongoDB, mtl, network, QuickCheck, quickcheck-text, scientific
, servant, servant-docs, servant-server, SHA, stdenv, text, time
, transformers-base, unordered-containers, vector, wai, wai-extra
, warp
}:
mkDerivation {
  pname = "kapi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bloodhound bson bytestring
    containers exceptions http-client http-types iso8601-time
    lifted-base monad-control mongoDB mtl network scientific servant
    servant-docs servant-server SHA text time transformers-base
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base bytestring containers wai wai-extra warp
  ];
  testHaskellDepends = [
    base bloodhound bson containers hspec hspec-core http-types
    iso8601-time QuickCheck quickcheck-text text time
  ];
  doHaddock = false;
  homepage = "https://github.com/gabesoft/kapi#readme";
  description = "Rest api server";
  license = stdenv.lib.licenses.bsd3;
}
