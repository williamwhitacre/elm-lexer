module Lexer
  ( Class
  , Lexeme
  , Lexer

  , class
  , combinatorClass
  , parserClass
  , lexer

  , lex

  , pattern
  , interpret
  ) where

{-| An easy to use lexer written using `Bogdanp/elm-combine` that provides a sufficient backbone for use
with LR(k) combinator generators, or as a standalone for simpler higher order pattern matching.

# Types
@docs Lexer, Lexeme, Class

# Construction
@docs lexer, class, parserClass, combinatorClass

# Lexing
@docs lex

# Primitive
@docs pattern, interpret

-}

import Combine as Cb
import String


{-| A lexeme class, consisting of a regex pattern and a transducer that will interpret the token
and spit out an associated lexeme. -}
type Class lex =
  Class
    { regexp : String
    , combinator : Maybe (Cb.Parser String)
    , parser : Maybe (Cb.Parser lex)
    , interpret' : String -> Result (List String) lex
    }


{-| A lexeme, including the original string token, the interpreted lexeme, the index of
occurrence, and the length. -}
type alias Lexeme lex =
  { token : String
  , lexeme : lex
  , index : Int
  , length : Int
  }


{-| A lexer. -}
type Lexer lex =
  Lexer
    { classes : List (Class lex)
    , lexemeChoiceParser : Cb.Parser (Lexeme lex)
    , combinator : Cb.Parser (List (Lexeme lex))
    }


{-| Create a new lexeme class from a Javascript style regex and an function to interpret a lexeme
from the matched token. -}
class : String -> (String -> Result (List String) lex) -> Class lex
class regexp interpret' =
  Class
    { regexp = regexp
    , combinator = Nothing
    , parser = Nothing
    , interpret' = interpret'
    }


{-| In the case that you wish to use a more advanced combinator in place of a pattern, you may do that
with this class. You can use this to create more powerful abstractions in your lexeme output.

_DEPRECIATED_
-}
combinatorClass : Cb.Parser String -> (String -> Result (List String) lex) -> Class lex
combinatorClass combinator interpret' =
  Class
    { regexp = ""
    , combinator = Just combinator
    , parser = Nothing
    , interpret' = interpret'
    }


{-| Use a combinator parser directly. The form of the lexeme itself may be turing recognizable in
this case. In this sense you are no longer strictly "lexing", but the output still provides the
typical useful token metadata for Lexeme record list produced the top level alternation. -}
parserClass : Cb.Parser lex -> Class lex
parserClass parser =
  Class
    { regexp = ""
    , combinator = Nothing
    , parser = Just parser
    , interpret' =
        Result.Err ["interpret' function unreachable in Lexer.Class produced by parserClass."]
        |> always
    }


{-| Get the pattern of a class. Note that this will return the empty string in the case that a
combinator is used instead. -}
pattern : Class lex -> String
pattern (Class class) =
  class.regexp


{-| Interpret a token. -}
interpret : Class lex -> String -> Result (List String) lex
interpret (Class class) input =
  class.interpret' input


patternParser_ : Class lex -> Cb.Parser lex
patternParser_ (Class {regexp, combinator, parser, interpret'}) =
  case parser of
    Just parser' -> parser'
    Nothing ->
      let
        combinator' =
          Maybe.map identity combinator
          |> Maybe.withDefault (Cb.regex regexp)

      in
        combinator' `Cb.andThen`
          (\smatch -> case interpret' smatch of
            Result.Ok lexeme -> Cb.succeed lexeme
            Result.Err errors -> Cb.fail errors
          )


captureLexeme_ : Cb.Parser lex -> Cb.Context -> (Result (List String) (Lexeme lex), Cb.Context)
captureLexeme_ lexParser {position, input} =
  let
    (result, context') =
      Cb.parse lexParser input

  in
    ( Result.map
        (\lexeme ->
          { lexeme = lexeme
          , token = String.left context'.position input
          , index = position
          , length = context'.position
          }
        ) result
    , { context'
      | position =
          position + context'.position
      }
    )


lexemeParser_ : Class lex -> Cb.Parser (Lexeme lex)
lexemeParser_ =
  patternParser_ >> captureLexeme_ >> Cb.primitive


{-| Create a lexer from a list of lexeme classes. -}
lexer : List (Class lex) -> Lexer lex
lexer classes =
  let
    lexemeChoiceParser =
      List.map lexemeParser_ classes
      |> Cb.choice

  in
    Lexer
      { classes = classes
      , lexemeChoiceParser = lexemeChoiceParser
      , combinator = Cb.many lexemeChoiceParser
      }


{-| Lex a string, producing a list of errors (if any) and a list of lexemes. -}
lex : Lexer lex -> String -> (List String, List (Lexeme lex))
lex (Lexer {combinator}) input =
  let
    (result, context) =
      Cb.parse combinator input

    completionErrors =
      if context.input /= "" then
        [ "Unrecognized token in input at " ++ toString context.position ++ "."
        , "Can't lex input: " ++ context.input
        ]
      else
        [ ]
  in
    case result of
      Result.Ok lexemes ->
        (completionErrors, lexemes)
      Result.Err errors ->
        (errors ++ completionErrors, [ ])
