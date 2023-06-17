module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html


convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        Markup.Heading n txt ->
            Html.h_ n txt
        
        Markup.Paragraph p ->
            Html.p_ p

        Markup.UnorderedList items ->
            Html.ul_ (map Html.p_ items)

        Markup.OrderedList items ->
            Html.ol_ (map Html.p_ items)

        Markup.CodeBlock lines ->
            Html.code_ (unlines lines)