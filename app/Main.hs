module Main (main) where

-- import Html
import Markup
import Convert
import Html


main :: IO ()
main = do
    txt <- readFile "test/sample.txt"
    let d = parse txt
    let h = convert "Hello" d
    writeFile "test/sample.html" (render h)