{-# LANGUAGE OverloadedStrings #-}

import Data.String.Conversions
import Data.Text

main :: IO ()
main = putStrLn $ cs text

text :: Text
text = "Hello, world!"
