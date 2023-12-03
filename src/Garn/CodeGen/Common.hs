module Garn.CodeGen.Common where

import Data.Char (isDigit)
import Data.Functor ((<&>))

sanitize :: String -> String
sanitize str
  | isDigit $ head str = sanitize $ "_" <> str
  | str `elem` tsKeywords = str <> "_"
  | otherwise =
      str <&> \case
        '+' -> '_'
        '-' -> '_'
        '.' -> '_'
        '/' -> '_'
        '@' -> '_'
        x -> x

tsKeywords :: [String]
tsKeywords =
  [ "arguments", -- Only in strict mode
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceOf",
    "interface",
    "new",
    "null",
    "private",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeOf",
    "typeof",
    "var",
    "void",
    "while",
    "with"
  ]
