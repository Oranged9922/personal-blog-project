module Main (main) where

-- import Html
import HsBlog.Html
import HsBlog.Convert
import HsBlog.Markup


main :: IO ()
main = do
    txt <- readFile "test/sample.txt"
    let d = parse txt
    let h = convert "Hello" d 
    writeFile "test/sample.html" (render h)