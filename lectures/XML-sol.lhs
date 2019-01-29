---
fulltitle: "Exercise: XML parsing"
date: November 8, 2017
---

In today\'s exercise you will use the definitions from the `Parsers` lecture to
build a simple parser for `XML` data.

> module Xml where

> import Control.Applicative (Alternative(..))
> import System.IO

We base this exercise on the definitions so far in lecture. Make sure that you
have downloaded the solution to the lecture.

> import Parsers (Parser, satisfy, char, string, doParse, filter)
>   -- note that this line imports these functions as well as the instance for Parser
>   -- for the Functor, Applicative and Alternative classes.

Your goal: produce this structured data from a string

> -- | A simplified datatype for storing XML
> data SimpleXML =
>           PCDATA  String
>         | Element ElementName [SimpleXML]
>       deriving Show

> type ElementName = String


First: the characters `/`, `<`, and `>` are not allowed to appear in tags or
PCDATA.

> reserved :: Char -> Bool
> reserved c = c `elem` ['/', '<', '>']

Use this definition to parse a maximal nonempty sequence of nonreserved characters:

> text :: Parser String
> 
> text = some (satisfy (not . reserved))
> 

~~~~{.haskell}
    Xml> doParse text "skhdjf"
    [("skhdjf","")]
    Xml> doParse text "akj<skdfsdhf"
    [("akj","<skdfsdhf")]
    Xml> doParse text ""
    []
~~~~

and then use this definition to parse nonreserved characters into XML.

> pcdata :: Parser SimpleXML
> 
> pcdata = PCDATA <$> text
> 

~~~~{.haskell}
    Xml> doParse pcdata "akj<skdfsdhf"
    [(PCDATA "akj","<skdfsdhf")]
~~~~

Parse an empty element, like `"<br/>"`

> emptyContainer :: Parser SimpleXML
> 
> emptyContainer = Element <$> (char '<' *> text <* string "/>") <*> pure []
> 

~~~~~{.haskell}
    Xml> doParse emptyContainer "<br/>sdfsdf"
    [(Element "br" [],"sdfsdf")]
~~~~~

Parse a container element consisting of an open tag, a sequence of
some content, and matching a closing tag.  (For example, `<br></br>` or
`<title>A midsummer night's dream</title>`.)  You do NOT need to make
sure that the closing tag matches the open tag.

> container :: Parser SimpleXML -> Parser SimpleXML
> 
> container p = Element <$> (char '<' *> text <* string ">")
>                       <*> many p
>                       <* (string "</" *> text <* string ">") 


~~~~~{.haskell}
    Xml> doParse (container pcdata) "<br></br>"
    [(Element "br" [],"")]
    Xml> doParse (container pcdata) "<title>A midsummer night's dream</title>"
    [(Element "title" [PCDATA "A midsummer night's dream"],"")]
     -- should also work, even though the tag is wrong
    Xml> doParse (container pcdata) "<title>A midsummer night's dream</br>"
    [(Element "title" [PCDATA "A midsummer night's dream"],"")]
~~~~~

Now put the above together to construct a parser for simple XML data:

> xml :: Parser SimpleXML
> 
> xml = pcdata <|> emptyContainer <|> container2 xml
> 

~~~~~{.haskell}
   Xml> doParse xml "<body>a</body>"
   [(Element "body" [PCDATA "a"],"")]
   Xml> doParse xml "<body><h1>A Midsummer Night's Dream</h1><h2>Dramatis Personae</h2>THESEUS, Duke of Athens.<br/>EGEUS, father to Hermia.<br/></body>"
  [(Element "body" [Element "h1" [PCDATA "A Midsummer Night's Dream"],Element "h2" [PCDATA "Dramatis Personae"],PCDATA "THESEUS, Duke of Athens.",Element "br" [],PCDATA "EGEUS, father to Hermia.",Element "br" []],"")]
~~~~~

Now let\'s try it on something a little bigger. How about [dream.html](dream.html)?

> -- | Run a parser on a particular input file
> parseFromFile :: Parser a -> String -> IO [(a,String)]
> parseFromFile parser filename = do
>   handle <- openFile filename ReadMode
>   str    <- hGetContents handle
>   return $ doParse parser str


~~~~~{.haskell}
    Xml> parseFromFile xml "dream.html"
~~~~~


Challenge: rewrite container so that it only succeeds when the closing tag matches
the opening tag.

> container2 :: Parser SimpleXML -> Parser SimpleXML
> 
> container2 p = (\(x,y,z) -> Element x y) <$> (Parsers.filter (\ (x,y,z) -> (x == z)) triple) where
>                 triple = (,,) <$> (char '<' *> text <* string ">")
>                               <*> many p
>                               <*> (string "</" *> text <* string ">")


~~~~~~{.haskell}
     Xml> doParse (container2 pcdata) "<title>A midsummer night's dream</title>"
    [(Element "title" [PCDATA "A midsummer night's dream"],"")]
    Xml> doParse (container2 pcdata) "<title>A midsummer night's dream</br>"
    []
~~~~~~