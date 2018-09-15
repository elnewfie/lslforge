module Language.Lsl.Internal.XmlCreate(emit,emitSimple,xmlEscape,emitList) where

emit :: String -> [(String,String)] -> [(String -> String)] -> String -> String
emit name attrs body =
    showString "<" . showString name .
    foldl (.) (id) (map (\ (n,v) -> showString " " . showString n . showString "=" . shows v) attrs) .
    showString ">" . (foldl (.) id body) . showString "</" . showString name . showString ">"

x = emit "root" [("id","one")] [
        emit "child" [] [showString "hello"],
        emit "child" [] [showString "world"]
    ]

emitSimple :: String -> [(String,String)] -> String -> String -> String
emitSimple name attrs body =
    emit name attrs [showString (xmlEscape body)]

emitList tag f list = emit tag [] (map f list)

xmlEscape [] = []
xmlEscape ('<':cs) = ('&':'l':'t':';':(xmlEscape cs))
xmlEscape ('>':cs) = ('&':'g':'t':';':(xmlEscape cs))
xmlEscape ('\"':cs) = ('&':'q':'u':'o':'t':';':(xmlEscape cs))
xmlEscape ('&':cs) = ('&':'a':'m':'p':';':(xmlEscape cs))
xmlEscape ('\'':cs) = ('&':'a':'p':'o':'s':';':(xmlEscape cs))
xmlEscape (c:cs) = c:(xmlEscape cs)
