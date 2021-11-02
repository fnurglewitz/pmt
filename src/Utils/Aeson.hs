module Utils.Aeson
  ( removeFieldLabelPrefix,
  )
where

import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Char as Char
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

uncapitalize :: String -> String
uncapitalize (first : rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do viceversa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At"),
        ("\\", "'Back_Slash"),
        ("<=", "'Less_Than_Or_Equal_To"),
        ("\"", "'Double_Quote"),
        ("[", "'Left_Square_Bracket"),
        ("]", "'Right_Square_Bracket"),
        ("^", "'Caret"),
        ("_", "'Underscore"),
        ("`", "'Backtick"),
        ("!", "'Exclamation"),
        ("#", "'Hash"),
        ("$", "'Dollar"),
        ("%", "'Percent"),
        ("&", "'Ampersand"),
        ("'", "'Quote"),
        ("(", "'Left_Parenthesis"),
        (")", "'Right_Parenthesis"),
        ("*", "'Star"),
        ("+", "'Plus"),
        (",", "'Comma"),
        ("-", "'Dash"),
        (".", "'Period"),
        ("/", "'Slash"),
        (":", "'Colon"),
        ("{", "'Left_Curly_Bracket"),
        ("|", "'Pipe"),
        ("<", "'LessThan"),
        ("!=", "'Not_Equal"),
        ("=", "'Equal"),
        ("}", "'Right_Curly_Bracket"),
        (">", "'GreaterThan"),
        ("~", "'Tilde"),
        ("?", "'Question_Mark"),
        (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
