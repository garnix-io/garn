{ mkDerivation
, aeson
, ansi-wl-pprint
, async
, base
, containers
, cradle
, directory
, filepath
, getopt-generics
, hspec
, hspec-discover
, hspec-golden
, http-client
, interpolate
, lens
, lens-aeson
, lens-regex-pcre
, lib
, mockery
, optparse-applicative
, pcre-heavy
, pretty-simple
, process
, shake
, silently
, string-conversions
, template-haskell
, temporary
, text
, unix
, wreq
, yaml
}:
mkDerivation {
  pname = "garn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson
    ansi-wl-pprint
    base
    containers
    directory
    getopt-generics
    interpolate
    optparse-applicative
    pretty-simple
    process
    shake
    string-conversions
    template-haskell
    temporary
    text
    unix
  ];
  executableHaskellDepends = [
    aeson
    ansi-wl-pprint
    base
    containers
    directory
    getopt-generics
    interpolate
    optparse-applicative
    pretty-simple
    process
    shake
    string-conversions
    template-haskell
    temporary
    text
    unix
  ];
  testHaskellDepends = [
    aeson
    ansi-wl-pprint
    async
    base
    containers
    cradle
    directory
    filepath
    getopt-generics
    hspec
    hspec-discover
    hspec-golden
    http-client
    interpolate
    lens
    lens-aeson
    lens-regex-pcre
    mockery
    optparse-applicative
    pcre-heavy
    pretty-simple
    process
    shake
    silently
    string-conversions
    template-haskell
    temporary
    text
    unix
    wreq
    yaml
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/garnix-io/garn#readme";
  license = lib.licenses.asl20;
}
