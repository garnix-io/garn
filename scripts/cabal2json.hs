{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Parsec
import Distribution.Types.Executable
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText
import System.Environment

main :: IO ()
main = do
  cabalFile <-
    getArgs >>= \x -> case x of
      [x] -> pure x
      _ -> fail "usage: cabal2json <cabal-file>"
  Just r <- parseGenericPackageDescriptionMaybe <$> BS.readFile cabalFile
  BSL.putStr $ A.encode $ toJSON (flattenPackageDescription r)
  putStrLn ""

toJSON :: PackageDescription -> A.Value
toJSON p =
  A.object
    [ "name" A..= (unPackageName $ pkgName (package p)),
      "description" A..= (fromShortText $ description p),
      "synopsis" A..= (fromShortText $ synopsis p),
      "executables" A..= (unUnqualComponentName . exeName <$> executables p)
    ]
