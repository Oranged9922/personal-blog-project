module HsBlog.Markup
    ( Document
    , Structure(..)
    , parse
    )
where

import Numeric.Natural
import Data.Maybe (maybeToList)

-- * Types
type Document
    = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Show, Eq)

-- * Parse
parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
    -- We are done
        [] ->
            maybeToList context
        
        -- Heading 1 Case
        ('*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

        -- Heading 2 Case
        ('*' : '*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 2 (trim line) : parseLines Nothing rest)

        -- Heading 3 Case
        ('*' : '*' : '*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 3 (trim line) : parseLines Nothing rest)

        -- Heading 4 Case
        ('*' : '*' : '*' : '*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 4 (trim line) : parseLines Nothing rest)

        -- Heading 5 Case
        ('*' : '*' : '*' : '*' : '*' : ' ' : line) : rest ->
            maybe id (:) context (Heading 5 (trim line) : parseLines Nothing rest)

        -- Unordered list case
        ('-' : ' ' : line) : rest ->
            case context of
                Just (UnorderedList list) ->
                    parseLines (Just (UnorderedList (list <> [trim line]))) rest
                
                _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

        -- Ordered list case
        ('#' : ' ' : line) : rest ->
            case context of
                Just (OrderedList list) ->
                    parseLines (Just (OrderedList (list <> [trim line]))) rest

                _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

        -- Code block case
        ('>' : ' ' : line) : rest ->
            case context of
                Just (CodeBlock code) ->
                    parseLines (Just (CodeBlock (code <> [line]))) rest

                _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

        -- Paragraph case
        currentLine : rest ->
            let
                line = trim currentLine
            in
                if line == ""
                    then
                        maybe id (:) context (parseLines Nothing rest)
                    else
                        case context of
                            Just (Paragraph paragraph) ->
                                parseLines (Just (Paragraph (paragraph <> " " <> line))) rest

                            _ -> maybe id (:) context (parseLines (Just (Paragraph line)) rest)

-- * Utilities
trim :: String -> String
trim = unwords . words