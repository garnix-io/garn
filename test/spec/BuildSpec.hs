{-# LANGUAGE QuasiQuotes #-}

module BuildSpec where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake (CmdOption (EchoStdout), StdoutTrim (..), cmd, cmd_)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  describe "build" $ withRunGarn $ do
    repoDir <- runIO getCurrentDirectory
    it "builds packages and creates a result link" $ \runGarn -> do
      writeHaskellProject repoDir
      _ <- runGarn ["build", "foo"]
      doesDirectoryExist "result" `shouldReturn` True
      StdoutTrim output <- cmd ("result/bin/garn-test" :: String)
      output `shouldBe` ("haskell test output" :: String)

    it "complains about packages that cannot be built" $ \runGarn -> do
      writeHaskellProject repoDir
      writeFile
        "Main.hs"
        [i|
          main :: IO ()
          main = "foo"
        |]
      output <- runGarn ["build", "foo"]
      stderr output `shouldContain` "Couldn't match type"
      exitCode output `shouldBe` ExitFailure 1

    it "allows to specify which sub-package to build" $ \runGarn -> do
      writeFile "garn.ts" $
        unindent
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts";

            export const project = garn.mkProject(
              { description: "" },
              {
                foo: garn.build("echo 'foo' > $out/build-artifact"),
                bar: garn.build("echo 'bar' > $out/build-artifact"),
              },
            );
          |]
      output <- runGarn ["build", "project.foo"]
      exitCode output `shouldBe` ExitSuccess
      readFile "result/build-artifact" `shouldReturn` "foo\n"
      output <- runGarn ["build", "project.bar"]
      exitCode output `shouldBe` ExitSuccess
      readFile "result/build-artifact" `shouldReturn` "bar\n"

    it "builds top-level packages" $ \runGarn -> do
      writeFile "garn.ts" $
        unindent
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts";

            export const p = garn.build("echo 'build output' > $out/build-artifact");
          |]
      output <- runGarn ["build", "p"]
      exitCode output `shouldBe` ExitSuccess
      readFile "result/build-artifact" `shouldReturn` "build output\n"

    it "shows a nice help for packages" $ \runGarn -> do
      writeFile "garn.ts" $
        unindent
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts";

            export const project = garn.mkProject(
              { description: "" },
              {
                short: garn.build("short command"),
                longer: garn.build`
                  # this is some longer build script:
                  bla bla bla
                `,
              },
            );
          |]
      output <- runGarn ["build"]
      stderr output `shouldMatch` "project.short \\s+ Builds short command"
      stderr output `shouldMatch` "project.longer \\s+ Builds # this is some longe\\.\\.\\."

    describe "addPackage" $ do
      it "builds packages within the project's default environment" $ \runGarn -> do
        writeFile
          "garn.ts"
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts"
            import { addToSetup } from "#{repoDir}/ts/environment.ts"

            const env = addToSetup(
              "sandboxed",
              garn.mkEnvironment(),
              garn.nix.nixStrLit`
                mkdir dist
                echo build-content > dist/build-artifact
              `,
            );
            export const project = garn.mkProject({
              description: "",
              defaultEnvironment: env,
            }, {})
              .addPackage("build", "mv dist/build-artifact $out/build-artifact");
          |]
        output <- runGarn ["build", "project"]
        readFile "result/build-artifact" `shouldReturn` "build-content\n"
        exitCode output `shouldBe` ExitSuccess

    describe ".build" $ do
      it "builds manually specified packages" $ \runGarn -> do
        writeFile
          "garn.ts"
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts"

            export const project = garn.mkProject(
              { description: "" },
              {
                package: garn.build("echo 'build-content' > $out/build-artifact"),
              },
            )
          |]
        output <- runGarn ["build", "project"]
        readFile "result/build-artifact" `shouldReturn` "build-content\n"
        exitCode output `shouldBe` ExitSuccess

      it "makes tools from the environment available" $ \runGarn -> do
        writeFile
          "garn.ts"
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts"
            import * as nix from "#{repoDir}/ts/nix.ts"

            export const project = garn.mkProject(
              { description: "" },
              {
                package:
                  garn
                    .emptyEnvironment
                    .withDevTools([garn.mkPackage(nix.nixRaw`pkgs.hello`, "hello")])
                    .build("hello > $out/build-artifact"),
              },
            )
          |]
        output <- runGarn ["build", "project"]
        readFile "result/build-artifact" `shouldReturn` "Hello, world!\n"
        exitCode output `shouldBe` ExitSuccess

      it "runs the environments setup steps" $ \runGarn -> do
        writeFile
          "garn.ts"
          [i|
            import * as garn from "#{repoDir}/ts/mod.ts"
            import * as nix from "#{repoDir}/ts/nix.ts"
            import { addToSetup } from "#{repoDir}/ts/environment.ts"

            export const project = garn.mkProject(
              { description: "" },
              {
                package: addToSetup(
                    "sandboxed",
                    garn.mkEnvironment(),
                    nix.nixStrLit`SETUP_VAR="hello from setup"`,
                  ).build("echo $SETUP_VAR > $out/build-artifact"),
              },
            )
          |]
        output <- runGarn ["build", "project"]
        readFile "result/build-artifact" `shouldReturn` "hello from setup\n"
        exitCode output `shouldBe` ExitSuccess

    it "includes untracked files when building packages" $ \runGarn -> do
      cmd_ "git init --initial-branch=main" (EchoStdout False)
      writeHaskellProject repoDir
      _ <- runGarn ["build", "foo"]
      doesDirectoryExist "result" `shouldReturn` True
      StdoutTrim output <- cmd ("result/bin/garn-test" :: String)
      output `shouldBe` ("haskell test output" :: String)

    it "does not warn about dirty git directories" $ \runGarn -> do
      cmd_ "git init --initial-branch=main" (EchoStdout False)
      writeFile "file" "foo"
      writeFile
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts";

          export const pkg = garn.build("echo built > $out/artifact");
        |]
      cmd_ "git add ."
      cmd_ "git commit -m" ["test commit message"] (EchoStdout False)
      writeFile "file" "bar"
      output <- runGarn ["build", "pkg"]
      stderr output `shouldNotMatch` "Git tree .* is dirty"
