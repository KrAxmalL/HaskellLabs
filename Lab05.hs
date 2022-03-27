{-# OPTIONS_GHC -Wall #-}
module Lab05 where

import Data.Char(isSpace, isDigit, isLetter)

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Function 1 -----------------------------------------
spaces :: String -> String
spaces [] = []
spaces [ch] = [ch | not (isSpace ch)]
spaces (ch:str) | not (isSpace ch) = ch:str
                | isSpace ch && not (isSpace (head str)) = str
                | otherwise = spaces str

-- Function 2.a ----------------------------------------- 
manyN :: String -> (String,String)
manyN str = findPrefixMatches str isSN ([], [])

isSN :: Char -> Bool
isSN ch = isLetter ch || isDigit ch || (ch == '.') || (ch == '-')

-- Function 2.b ----------------------------------------- 
value :: String -> (String,String)
value str = findPrefixMatches str isCV ([], [])

isCV :: Char -> Bool
isCV ch = ch /= '"'

-- Function 2.c ----------------------------------------- 
manyT :: String -> (String,String)
manyT str = findPrefixMatches str isST ([], [])

isST :: Char -> Bool
isST ch = ch /= '<' && ch /= '>'

-- Function 3.a -----------------------------------------
name :: String -> Maybe(String,String)
name [] = Nothing
name str | isLetter (head str) = Just (manyN str)
         | otherwise = Nothing

-- Function 3.b -----------------------------------------
text :: String -> Maybe(String,String)
text [] = Nothing
text str | isST (head str) = Just (manyT str)
         | otherwise = Nothing

-- Function 3.c -----------------------------------------
fullValue :: String -> Maybe(String,String)
fullValue [] = Nothing
fullValue str | not (isCV (head str)) = getFullValue (value (tail str))
              | otherwise = Nothing
              where getFullValue :: (String,String) -> Maybe(String,String)
                    getFullValue p | null (snd p) = Nothing
                                   | head (snd p) == '"' = Just(fst p, tail (snd p))
                                   | otherwise = Nothing

-- Function 4.a -----------------------------------------
attrib :: String -> Maybe ((String,String), String)
attrib [] = Nothing
attrib str = case name str of
  Just(nm, left) -> case spaces left of
    '=':fullValLeft -> case fullValue (spaces fullValLeft) of
      Just(finalFulVal, allLeft) -> Just ((nm, finalFulVal), spaces allLeft)
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- Function 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String)
manyAtt [] = Just([], [])
manyAtt str | isSpace (head str) = Nothing
            | otherwise = manyAttHelper ([], str)
                    where manyAttHelper :: (Attributes,String) -> Maybe (Attributes,String)
                          manyAttHelper p = case attrib (snd p) of
                            Nothing -> Just p
                            Just(attr, left) -> manyAttHelper (fst p ++ [attr], left)

-- Function 5.a -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag [] = Nothing
begTag str = case str of
  ('<':otherStr) -> case name otherStr of
    Just(nm, left) -> case left of
      (ch:findManyAttr) -> case ch of
        '>' -> Just((nm, []), findManyAttr)
        ' ' -> case manyAtt findManyAttr of
          Just(attribs, allLeft) -> case allLeft of
             ('>': finalLeft) -> Just((nm, attribs), finalLeft)
             _ -> Nothing
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- Function 5.b -----------------------------------------
endTag :: String -> Maybe (String,String)
endTag [] = Nothing
endTag str = case str of
  ('<':left) -> case left of
    ('/': findName) -> case name findName of
      Just(nm, findEndQuot) -> case findEndQuot of
        ('>':finalLeft) -> Just(nm, finalLeft)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- Function 6.a -----------------------------------------
element :: String -> Maybe (XML,String)
element str = case begTag str of
  Just(tagVal, left) -> case manyXML left of
    Just(fullXml, leftForCloseTag) -> case endTag leftForCloseTag of
      Just(closeTagName, finalLeft) -> if closeTagName == fst tagVal then Just(Element closeTagName (snd tagVal) fullXml, finalLeft) 
                                                                     else Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- Function 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml str = case element str of
  Just(foundXml, leftAfterXml) -> Just(foundXml, leftAfterXml)
  Nothing -> case text str of
    Just(foundText, left) -> Just(Text foundText, left)
    _ -> Nothing

-- Function 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML str | null str = Just([], [])
            | otherwise = manyXMLHelper ([], str)
                    where manyXMLHelper :: ([XML],String) -> Maybe([XML],String)
                          manyXMLHelper p = case xml (snd p) of
                                              Just(currLs, left) -> manyXMLHelper (fst p ++ [currLs], left)
                                              Nothing -> case snd p of
                                                          ('<':'/':_) -> Just p
                                                          _ -> Nothing

-- Function 7 -----------------------------------------
fullXML :: String -> Maybe XML
fullXML str = case element (spaces str) of
  Just(fullDocument, left) -> case spaces left of
    [] -> Just fullDocument
    _ -> Nothing
  _ -> Nothing

--------------------------------------------
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]


findPrefixMatches :: String -> (Char -> Bool) -> (String, String) -> (String, String)
findPrefixMatches [] _ p = p
findPrefixMatches [ch] chPred p = if chPred ch then (fst p ++ [ch], snd p) else (fst p, snd p ++ [ch])
findPrefixMatches (ch:s) chPred p | chPred ch = findPrefixMatches s chPred (fst p ++ [ch], snd p)
                                  | otherwise = (fst p, snd p ++ (ch:s))

fromJust :: Maybe a -> a
fromJust val = case val of
  Just insideVal -> insideVal
  _ -> error "value is Nothing!"
