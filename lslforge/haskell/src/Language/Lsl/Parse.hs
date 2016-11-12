{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{-# OPTIONS_GHC -XQuasiQuotes #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
module Language.Lsl.Parse(
        alternateScriptParser,
        alternateModuleParser,
        parseScript,
        parseModule,
        exprParser,
        parseType,
        parseScriptFromString,
        parseModuleFromString,
        parseScriptFromStringAQ,
        parseModuleFromStringAQ,
        ParseError,
        SourcePos,
        errorMessages,
        errorPos,
        showErrorMessages,
        sourceLine,
        sourceColumn,
        sourceName
    ) where

import Control.Applicative hiding (many) -- use parsec's many (s.b. equivalent)
import Control.Monad(ap,mplus,mzero)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char(digitToInt)
import Data.List(intersperse,intercalate)
import Data.Maybe(catMaybes)
import Language.Lsl.Internal.Pragmas(Pragma(..))
import Language.Lsl.Syntax(Expr(..),Statement(..),Func(..),FuncDec(..),
    Handler(Handler),State(..),Ctx(..),TextLocation(..),SourceContext(..),
    LSLType(..),Component(..),Var(..),LModule(..),LSLScript(..),GlobDef(..),
    goodHandlers,nullCtx)
import Text.ParserCombinators.Parsec hiding (State,(<|>))
import qualified Text.ParserCombinators.ParsecExtras.Token as P
import Text.ParserCombinators.ParsecExtras.Language(javaStyle, emptyDef)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
     
data ParseState = ParseState {
    atStart :: !Bool,
    initialComment :: !String,
    parseAntiQuotations :: !Bool,
    pendingPragmas :: ![Pragma],
    trailingWS :: !String,
    leadingWS :: !String,
    preWSPos :: !(Maybe SourcePos) }
    
getAQState = getState >>= return . parseAntiQuotations

newAQState = ParseState { 
    atStart = True, 
    initialComment = "",
    parseAntiQuotations = True,
    pendingPragmas = [],
    trailingWS = "",
    leadingWS = "",
    preWSPos = Nothing }
newNoAQState = ParseState { 
    atStart = True,
    initialComment = "",
    parseAntiQuotations = False,
    pendingPragmas = [],
    trailingWS = "",
    leadingWS = "",
    preWSPos = Nothing }

data WS = WSSimple String | WSSingle String | WSMulti String deriving Show
    
custWS simpleSpace oneLineComment multiLineComment = do
    startPos <- getPosition
    st <- getState
    ws <- many ((WSSimple <$> simpleSpace) <|> 
             (WSSingle <$> oneLineComment) <|> 
             (WSMulti <$> multiLineComment) <?> "")
    let (trailing,leading) = if atStart st then ([],ws) else extractTrailing ws
    let pragmas = foldl extractPragma [] leading
    let (initComment,leading') = if atStart st
            then findInitialComment True [] leading
            else (initialComment st, leading)
    setState st { atStart = False,
               initialComment = initComment,
               pendingPragmas = pragmas, 
               trailingWS = wsCat trailing,
               leadingWS = wsCat leading',
               preWSPos = Just startPos }
    where 
        wsCat = concatMap showWS
        showWS (WSSimple txt) = txt
        showWS (WSSingle txt) = "//" ++ txt
        showWS (WSMulti txt) = "/*" ++ txt ++ "*/"
        extractTrailing [] = ([],[])
        extractTrailing (WSSimple txt:rest) = case break (=='\n') txt of
               (ttxt,[]) -> let (t,rest') = extractTrailing rest in (WSSimple ttxt:t,rest')
               ([],mtxt) -> ([],WSSimple mtxt:rest)
               (ttxt,mtxt) -> ([WSSimple ttxt],WSSimple mtxt:rest)
        extractTrailing (ws@(WSSingle txt):rest) = ([ws],rest)
        extractTrailing (ws@(WSMulti txt):rest) | '\n' `elem` txt = ([ws],rest)
                                               | otherwise       = let (t,rest') = extractTrailing rest in (ws:t,rest')
        extractPragma ps (WSSingle txt) = maybe ps (:ps) (parsePragma txt)
        extractPragma ps _ = ps
        findInitialComment :: Bool -> [WS] -> [WS] -> (String,[WS])
        findInitialComment _ rleading [] = ("",reverse rleading)
        findInitialComment onNewLine rleading (ws@(WSSimple txt):wss)
           | null txt = findInitialComment onNewLine rleading wss
           | (onNewLine && newlines txt > 0) || newlines txt > 1 = (wsCat (reverse (ws:rleading)), wss)
           | otherwise = findInitialComment (last txt == '\n') (ws:rleading) wss
        findInitialComment onNewLine rleading (ws:wss) = findInitialComment False (ws:rleading) wss
        newlines s = length [ c | c <- s, c == '\n']
parsePragma :: String -> Maybe Pragma
parsePragma txt =
  case parse parser "" txt of
      Left _ -> Nothing
      Right p -> Just p
   where 
       pragmaStyle = emptyDef { P.reservedNames = ["pragma","inline","noinlining"], P.commentLine = "--" }
       lexer :: P.TokenParser ()
       lexer = P.makeTokenParser pragmaStyle
       reserved = P.reserved lexer
       ws = P.whiteSpace lexer
       parser = do
           ws
           reserved "pragma"
           pragma <- (reserved "inline" >> return PragmaInline) <|> (reserved "noinlining" >> return PragmaNoInline)
           eof
           return pragma
    
getPreWSPos = getState >>= return . preWSPos

-- get the position after the last non-whitespace character parsed so far
getEndPosition = getPreWSPos >>= maybe getPosition return

getTrailingWS = getState >>= return . trailingWS
getLeadingWS = getState >>= return . leadingWS

-- define basic rules for lexical analysis
lslStyle = javaStyle { 
    P.reservedOpNames= ["*","/","+","++","-","--","^","&","&&",
                        "|","||","==","=","!=","<","<=",">=",">",
                        "<<",">>","!","%","~","@","+=","-=","*=","/=","%="],
    P.reservedNames = ["state","default","string","integer","list","vector","rotation","key","float","if","else",
                      "while","for","do","jump","return","default", "$import", "$module","quaternion"],
    P.caseSensitive = True,
    P.identStart = letter <|> char '_',
    P.opLetter = oneOf "*/+:!#$%&*+/=?@\\^|-~",
    P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.custWhiteSpace = Just custWS }
    
lexer :: P.TokenParser ParseState
lexer = P.makeTokenParser lslStyle

identLetter = P.identLetter lslStyle
               
identifier       = P.identifier lexer
reserved         = P.reserved lexer
reservedOp       = P.reservedOp lexer
symbol           = P.symbol lexer
lexeme           = P.lexeme lexer
whiteSpace       = P.whiteSpace lexer
parens           = P.parens lexer 
braces           = P.braces lexer
brackets         = P.brackets lexer
semi             = P.semi lexer
comma            = P.comma lexer
dot              = P.dot lexer
commaSep         = P.commaSep lexer

openParen = lexeme $ char '('
closeParen = lexeme $ char ')'
openAngle = lexeme $ char '<'
closeAngle = lexeme $ char '>'

decimalFraction w = char '.' >> fracPart True w
                       
fracPart reqDigit w = do 
    digits <- (if reqDigit then many1 else many) digit 
        <?> "fractional part of decimal"
    p <- option 1.0 expon
    return $ p * (w + (foldr (\ b d -> (b + d) / 10.0) 0 
        $ map (fromIntegral.digitToInt) digits))

expon :: GenParser Char st Double
expon = do 
    oneOf "eE"
    s <- option '+' (oneOf "+-")
    let k x = if s == '+' then x else 1/x
    digits <- many1 digit <?> "exponent"
    let p = foldl (\ b d -> b * 10 + d) 0 $ map digitToInt digits
    return (k (10^p))

hex = do 
    oneOf "xX"
    digits <- many1 hexDigit <?> "hex digit"
    return (foldl (\ b d -> b * 16 + d) 0 $ map digitToInt digits)
-- lsl doesn't support octal!
-- oct = do oneOf "oO"
--          digits <- many1 octDigit <?> "octal digit"
--          return (foldl (\ b d -> b * 8 + d) 0 $ map digitToInt digits)

-- Like Parsec 'naturalOrFloat', but accept <digits>. and .<digits><exp> as valid floating point numbers
naturalOrFloat = lexeme natOrFloat <?> "number"

natOrFloat =   Right <$> decimalFraction 0.0
           <|> try (char '0' >> option  (Left 0) prefZeroNum)
           <|> decimalOrFloat

prefZeroNum = Left <$> hex -- (hex >>= return . Left)
          <|> Right <$> decimalFraction 0.0
          <|> decimalOrFloat
          
decimalOrFloat = do
    wholeDigits <- many1 digit <?> "number"
    let w = foldl (\ b d -> b * 10 + d) 0 $ map digitToInt wholeDigits
    mf <- option Nothing (char '.' >> option (fromIntegral w) (fracPart False (fromIntegral w)) >>= return . Just)
    case mf of
        Nothing -> try ( expon >>= \ p -> return $ Right (fromIntegral w * p) ) <|> (return $ Left w)
        Just f -> return $ Right f

stringLiteral   = lexeme (
                      do{ str <- between (char '"')                   
                                         (char '"' <?> "end of string")
                                         (many stringChar) 
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "literal string")

stringChar :: CharParser st (Maybe Char)
stringChar = do{ c <- stringLetter; return (Just c) }
         <|> try stringEscape
         <|> (char '\\' >> return (Just '\\'))
         <?> "string character"
                
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\'))

stringEscape = do
    char '\\'
    choice [char 't' >> return (Just '\t'),
            char 'n' >> return (Just '\n'),
            char '\"' >> return (Just '\"'),
            char '\\' >> return (Just '\\')]

--- EXPRESSION PARSING -------------------------------------------------------

combineExprs e [] = e
combineExprs e0 ((p0,p1,op,e1):rest) = combineExprs (op p0 p1 e0 e1) rest

isFollowedBy p = lookAhead p >> return ()
    
reservedOp' name = 
    lexeme $ try $
    do{ string name
      ; ( isFollowedBy (oneOf "-!~") <|> notFollowedBy (P.opLetter lslStyle)) <?> ("end of " ++ show name)
      }

opmatch s f = (reservedOp s >> return f)
opmatch' s f = (reservedOp' s >> return f)
op0 = ((opmatch' "*" mulop) <|> (opmatch' "/" divop) <|> (opmatch' "%" modop)) <?> "operator"
op1 = ((opmatch' "+" addop) <|> (opmatch "-" subop)) <?> "operator"
op2 = ((opmatch' "<<" shiftlop) <|> (opmatch' ">>" shiftrop)) <?> "operator"
op3 = (choice [opmatch' "<" ltop, opmatch' "<=" leop, opmatch' ">" gtop, opmatch' ">=" geop]) <?> "operator"
op4 = ((opmatch' "==" eqop) <|> (opmatch' "!=" neop)) <?> "operator"

opAndExpr opf ef = mkIt <$> getPosition <*> opf <*> getPosition <*> ef
    where mkIt p0 op p1 e = (p0,p1,op,e)
    
exprChain opf ef = do e0 <- ef
                      rest <- many (opAndExpr opf ef)
                      return $ combineExprs e0 rest
                      
mulExpr = exprChain op0 expr2
addExpr = exprChain op1 mulExpr
shiftExpr = exprChain op2 addExpr
relExpr = combineExprs <$> shiftExpr <*> 
    (try (many (try (opAndExpr op3 shiftExpr))) <|> return [])
eqExpr = exprChain op4 relExpr
bandExpr = exprChain (opmatch' "&" bandop <?> "operator") eqExpr
xorExpr = exprChain (opmatch' "^" xorop <?> "operator") bandExpr
borExpr = exprChain (opmatch' "|" borop <?> "operator") xorExpr
andExpr = exprChain (opmatch' "&&" andop <?> "operator") borExpr
orExpr = exprChain (opmatch' "||" orop <?> "operator") andExpr

bop op pos0 pos1 e0 e1 = 
    let ctx = combineContexts (srcCtx e0, pos0, pos1, srcCtx e1) in Ctx ctx $ op e0 e1 

mulop pos0 pos1 = bop Mul pos0 pos1
divop pos0 pos1 = bop Div pos0 pos1
modop pos0 pos1 = bop Mod pos0 pos1
addop pos0 pos1 = bop Add pos0 pos1
subop pos0 pos1 = bop Sub pos0 pos1
shiftlop pos0 pos1 = bop ShiftL pos0 pos1
shiftrop pos0 pos1 = bop ShiftR pos0 pos1
ltop pos0 pos1 = bop Lt pos0 pos1
leop pos0 pos1 = bop Le pos0 pos1
gtop pos0 pos1 = bop Gt pos0 pos1
geop pos0 pos1 = bop Ge pos0 pos1
eqop pos0 pos1 = bop Equal pos0 pos1
neop pos0 pos1 = bop NotEqual pos0 pos1
bandop pos0 pos1 = bop BAnd pos0 pos1
xorop pos0 pos1 = bop Xor pos0 pos1
borop pos0 pos1 = bop BOr pos0 pos1
andop pos0 pos1 = bop And pos0 pos1
orop pos0 pos1 = bop Or pos0 pos1

structAccess =
    (,) <$> ctxify identifier 
        <*> (char '.' >> (stringToComponent . (:[])) <$> lexeme (oneOf "xyzs"))
-- structAccess = do id <- ctxify identifier
--                   whiteSpace
--                   dot
--                   whiteSpace
--                   c <- oneOf "xyzs"
--                   whiteSpace
--                   return (id,stringToComponent [c])
                      
var = try structAccess
   <|> (,) <$> ctxify identifier <*> pure All

assignment = ctxify $ 
             do v <- var
                op <- choice [reservedOp' "+=" >> (return IncBy),
                              reservedOp' "-=" >> (return DecBy),
                              reservedOp' "*=" >> (return MulBy),
                              reservedOp' "/=" >> (return DivBy),
                              reservedOp' "%=" >> (return ModBy),
                              reservedOp' "="  >> (return Set)] <?> "assignment operator"
                e <- expr
                return $ op v e

stringToComponent "" = All
stringToComponent "x" = X
stringToComponent "y" = Y
stringToComponent "z" = Z
stringToComponent "s" = S

listExpr = ListExpr <$> ((brackets $ commaSep expr) <?> "list expression")
              
-- there's a conflict between a relational expression embedded in the last component of a vector/rotation and the
-- the normal end of the expression... in particular:
-- v = <1,1,1 > -<1,2,3>>; 
-- is not a syntax error (but is a type error), but you can't tell the difference between that and
-- v = <1,1,1 > -<1,2,3>;
-- which is the difference between two vectors (which is both syntactically correct and type correct) until you
-- parse the ';'.  So the complexity of the grammar here exists to disambiguate these cases (which requires a
-- bit of lookahead...)
structStart = (,) <$> (openAngle >> expr) <*> (comma >> expr) <* comma
                 
vecRotExpr = do (x,y) <- structStart 
                (try (rotTail x y)  <|> vecTail x y)
              
rotTail x y = RotExpr <$> pure x <*> pure y <*> expr <*> (comma >> tailStruct)
vecTail x y = VecExpr <$> pure x <*> pure y <*> tailStruct
       
tailStruct = try ( expr <* closeAngle) <|> shiftExpr <* closeAngle

callExpr = Call <$> ctxify identifier <*> parens (commaSep expr) 
              
castExpr = 
    ctxify $ Cast <$> parens typeName <*> choice [try postfixExpr, atomicExpr]

ctxify f = do
    pragmas <- getState >>= return . pendingPragmas
    pre <- getLeadingWS
    pos0 <- getPosition
    v <- f
    pos1 <- getEndPosition
    post <- getTrailingWS
    return $ Ctx (pos2Ctx (pos0,pos1) pre post pragmas) v

notExpr = ctxify (((lexeme . char) '!' <?> "prefix operator") >> expr2 >>= return . Not)
invExpr = ctxify (((lexeme . char) '~' <?> "prefix operator") >> expr2 >>= return . Inv)
negExpr = ctxify (((lexeme . char) '-' <?> "prefix operator") >> expr2 >>= return . Neg)

atomicExpr = getAQState >>= \ aq ->
        (ctxify $
            try ( do 
                m <- option 1 (reservedOp "-" >> return (-1))
                n <- naturalOrFloat
                return $ case n of
                    Left nat -> (IntLit $ m * fromIntegral nat)
                    Right flt -> (FloatLit $ fromRational $ toRational m * toRational flt)))
    <|> ctxify (StringLit <$> stringLiteral)
    <|> aqParse aq (ctxify $ reservedOp "$string:" >> AQString <$> identifier)
    <|> aqParse aq (ctxify $ reservedOp "$integer:" >> AQInteger <$> identifier)
    <|> aqParse aq (ctxify $ reservedOp "$key:" >> AQKey <$> identifier)
    <|> aqParse aq (ctxify $ reservedOp "$float:" >> AQFloat <$> identifier)
    <|> ctxify listExpr
    <|> ctxify vecRotExpr
    <|> (ctxify $ try callExpr)
    <|> (ctxify $ Get <$> var)
    <|> parens expr

aqParse False _ = mzero
aqParse True p = p
         
postfixExpr = ctxify $ do
    v <- var
    (choice [reservedOp' "++" >> return (PostInc v),
            reservedOp' "--" >> return (PostDec v)] <?> "postfix operator")
    --return $ f v
prefixExpr = ctxify (
    choice [reservedOp "--" >> PreDec <$> var,
            reservedOp "++" >> PreInc <$> var] <?> "prefix operator")
                            
unaryExpr = choice [try prefixExpr,notExpr,invExpr, negExpr,try castExpr,atomicExpr]

expr2 :: GenParser Char ParseState (Ctx Expr)
expr2 = choice [try assignment, try postfixExpr, unaryExpr]

expr1 = orExpr

expr :: GenParser Char ParseState (Ctx Expr)
expr = choice [try assignment, expr1]

exprParser :: String -> Either ParseError (Ctx Expr)
exprParser text = runParser (whiteSpace>>expr) newNoAQState "" text

optionalExpr = option Nothing (Just <$> expr)

------------------------------------------------------------------------------
-- STATEMENT PARSING

statements = many (ctxify statement)
             
statement = declStatement
        <|> returnStatement
        <|> jumpStatement
        <|> labelStatement
        <|> exprStatement
        <|> blockStatement
        <|> nullStatement
        <|> whileStatement
        <|> ifElseStatement
        <|> stateStatement
        <|> doWhileStatement
        <|> forStatement

optionalInit = option Nothing (reservedOp' "=" >> Just <$> expr)

declStatement = mkDecl <$> typeName <*> identifier <*> optionalInit <* semi
    where mkDecl t nm init = Decl (Var nm t) init
          
integerType     = (reserved "integer" >> return LLInteger)
floatType       = (reserved "float" >> return LLFloat)
keyType         = (reserved "key" >> return LLKey)
vectorType      = (reserved "vector" >> return LLVector)
stringType      = (reserved "string" >> return LLString)
rotationType    = ((reserved "rotation"<|>reserved "quaternion") >> return LLRot)
listType        = (reserved "list" >> return LLList)

typeName = choice [integerType,floatType,keyType,vectorType,stringType,rotationType,listType]

returnStatement = reserved "return" >> Return <$> optionalExpr <* semi

jumpStatement = reserved "jump" >> Jump <$> identifier <* semi

labelStatement = reservedOp "@" >> Label <$> identifier <* semi

exprStatement = Do <$> expr <* semi

blockStatement = Compound <$> braces statements

nullStatement = (semi >> return NullStmt)

whileStatement = reserved "while" >> While <$> parens expr <*> ctxify statement

ifElseStatement = reserved "if" >> If <$> parens expr <*> ctxify statement 
    <*> option (nullCtx NullStmt) (reserved "else" >> ctxify statement)
                    
stateStatement = reserved "state" >> StateChange 
    <$> (identifier <|> (reserved "default" >> return "default")) <* semi

forStatement = reserved "for" >> openParen
    >> For <$> commaSep expr <*> (semi >> optionalExpr)
        <*> (semi >> commaSep expr) <*> (closeParen >> ctxify statement)

doWhileStatement = reserved "do" >> DoWhile <$> ctxify statement 
    <*> (reserved "while" >> parens expr) <* semi

parseType text = runParser typeName newNoAQState "" text
------------------------------------------------------------
-- HANDLER Parsing

typeParser LLInteger = integerType
typeParser LLFloat = floatType
typeParser LLList = listType
typeParser LLString = stringType
typeParser LLVector = vectorType
typeParser LLKey = keyType
typeParser LLRot = rotationType

-- from a list of parameter types, generate a parser that parses a comma
-- separated list of parameter declarations.  (Since we know what types
-- the parameters must be for any handler, we can catch the type errors
-- at parse time).
hparams ts = catMaybes <$> 
    (sequence $ intersperse (comma >> return Nothing) (map hparam ts))
   where hparam theType = 
            Just <$> (ctxify $ mkVar <$> typeParser theType <*> identifier)
         mkVar t id = Var id t

handlerName name = try (symbol name <* notFollowedBy identLetter)

handler' name types =  ctxify $
    Handler <$> ctxify (handlerName name) <*> parens (hparams types)
        <*> braces statements
 
handler = choice $ map (\ (n,ts) -> handler' n ts) goodHandlers

-------------------------------------------------------------
-- STATE parsing

stateName = choice [reserved "default" >> return "default" , reserved "state" >> identifier]

stateDecl = ctxify $ State <$> ctxify stateName <*> braces (many handler)

stateDecls = many stateDecl

--------------------------------------------------------------
varOrFunc =   do pos0 <- getPosition
                 pre <- getLeadingWS
                 pragmas <- getState >>= return . pendingPragmas
                 (Ctx ctx (t,id)) <- ctxify $ do
                     t <- try typeName
                     id <- ctxify identifier <?> "identifier"
                     return (t,id)
                 choice [func t id pragmas pos0 pre, gvar ctx t id]
          <|> do pos0 <- getPosition
                 pre <- getLeadingWS
                 pragmas <- getState >>= return . pendingPragmas
                 id <- ctxify identifier <?> "identifier"
                 func LLVoid id pragmas pos0 pre
func t id pragmas pos0 pre = 
                         do ps <- parens params
                            stmts <- braces statements
                            pos1 <- getEndPosition
                            post <- getTrailingWS
                            return $ GF $ Ctx (pos2Ctx (pos0, pos1) pre post pragmas) $ Func (FuncDec id t ps) stmts
---------------------------------------------------------------
-- GLOBAL VARIABLES parsing
gvar ctx t (Ctx _ id) = do mexpr <- optionalInit
                           semi
                           return $ GV (Ctx ctx (Var id t)) mexpr
--------------------------------------------------------------
-- FUNCTION parsing

param = ctxify $
        do t <- typeName
           id <- identifier
           return $ Var id t
params = commaSep param

----------------------------------------------------------------
-- IMPORT (meta-lsl directive) parsing

gimport = do reserved "$import" <?> "$import keyword"
             ids <- (ctxify (char '$' >> identifier >>= return . (:[]) . ("$"++)))
                 <|>(ctxify $ sepBy identifier dot)
             let id = fmap (intercalate ".") ids
             let bindv = (,) <$> identifier <*> (reservedOp "=" >> identifier)
             bindings <- option [] $ parens $ commaSep bindv
             prefix <- option "" identifier
             semi
             return $ GI id bindings prefix
---------------------------------------------------------------------------------------------------
-- all globals parsing             
global = choice [gimport,varOrFunc]
globals = many global

lslParser = do whiteSpace
               globs <- globals
               ss <- stateDecls
               eof
               comment <- initialComment <$> getState
               return $ LSLScript comment globs ss

moduleParser = whiteSpace >> reserved "$module" >> 
    mkModule <$> option [] (parens params) <*> globals <* eof
    where mkModule fvars globs = LModule globs fvars

-- error recovering parser
-- this section implements a parser with limited error recovery, based on the parsers
-- defined above.  Parsec doesn't have much built-in support for error recovery, so
-- it seems you have to build error recovery 'around' the parsec parsers.

-- | the parse result represents the rest of the input (unparsed), where you left off
--   parsing, and, the parsed value.
data PResult a = PResult {
    resultInput :: !String,
    resultPosition :: !SourcePos,
    resultItem :: !a }

type PartialParser st a = GenParser Char st (PResult a)

withRest :: CharParser st a -> SourcePos -> PartialParser st a
withRest a p = do
    setPosition p
    v <- a
    st <- getParserState
    return PResult { resultInput = stateInput st, resultPosition = statePos st, resultItem = v }

-- parse as many globals then states without failing
parseGreedy1 = withRest $
    (do try whiteSpace
        gs <- many $ try global
        ss <- many $ try stateDecl
        return (gs,ss)) <|> return ([],[])
-- parse one global or state
parseStrict1 = withRest $
    do whiteSpace
       v <- choice [global >>= return . Left, stateDecl >>= return . Right]
       return v
parsePartial = withRest $
    ctxify $ 
      do whiteSpace
         name <- ctxify stateName
         lexeme (char '{')
         hs <- many $ try handler
         return (State name hs)
parseResume = withRest $ ctxify (
    (do whiteSpace
        hs <- many $ try handler
        lexeme (char '}')
        ss <- many $ try stateDecl
        return (hs,ss)) <|> -- failed to find end of state
    (do whiteSpace
        s <- stateDecl
        ss <- many $ try stateDecl
        return ([],s:ss)))
parseGreedy2 = withRest $
    (do try whiteSpace
        many $ try stateDecl) <|> return []
parseStrict2 = withRest stateDecl

alternateScriptParser srcName string = goGreedy1 False ([],[],[]) (initialPos "") string
    where doParse p pos = runParser (p pos) newNoAQState srcName
          goGreedy1 _ (gs,ss,errs) pos [] = (LSLScript "" gs ss, reverse errs)
          goGreedy1 skipErr (gs,ss,errs) pos s =
              case doParse parseGreedy1 pos s of
                 Left err -> error ("parseGreedy1 should never return an error, but returned: " ++ show err)
                 Right (PResult [] _ (gs',ss')) -> (LSLScript "" (gs ++ gs') (ss ++ ss'),errs)
                 Right (PResult rest pos' (gs',ss')) -> goStrict1 False (gs ++ gs',ss ++ ss',errs) pos' rest
          goStrict1 skipErr (gs,ss,errs) _ [] = (LSLScript "" gs ss,reverse errs)
          goStrict1 skipErr (gs,ss,errs) pos s@(c:cs) = 
              case doParse parseStrict1 pos s of
                 Left err -> goPartial1 True (gs,ss,if skipErr then errs else err:errs) pos s
                 Right (PResult rest pos' (Left g)) -> goGreedy1 False (gs ++ [g],ss,errs) pos' rest
                 Right (PResult rest pos' (Right s)) -> goGreedy2 False (gs,ss ++ [s], errs) pos' rest
          goPartial1 skipErr (gs,ss,errs) _ [] = (LSLScript "" gs ss,reverse errs)
          goPartial1 skipErr (gs,ss,errs) pos s@(c:cs) =
              case doParse parsePartial pos s of
                  Left err -> goStrict1 True (gs,ss,if skipErr then errs else err:errs) (updatePosChar pos c) cs
                  Right (PResult rest pos' state) -> goResume True state (gs,ss, errs) pos' rest
          goResume skipErr state (gs,ss,errs) _ [] = (LSLScript "" gs (ss ++ [state]),reverse errs)
          goResume skipErr state@(Ctx c0 (State nm hs)) (gs,ss,errs) pos s@(c:cs) =
              case doParse parseResume pos s of
                  Left err -> goResume True state (gs,ss,if skipErr then errs else err:errs) (updatePosChar pos c) cs
                  Right (PResult rest pos' (Ctx c1 (hs',ss'))) -> 
                      goGreedy2 False (gs,ss ++ (Ctx (combineContexts1 c0 c1) (State nm (hs ++ hs')):ss'),errs) pos' rest
          goGreedy2 skipErr (gs,ss,errs) _ [] = (LSLScript "" gs ss,reverse errs)
          goGreedy2 skipErr (gs,ss,errs) pos s =
              case doParse parseGreedy2 pos s of
                 Left err -> error ("parseGreedy2 parser should never return an error, but returned: " ++ show err)
                 Right (PResult [] _ ss') -> (LSLScript "" gs (ss ++ ss'),reverse errs)
                 Right (PResult rest pos' ss') -> goStrict2 True (gs,ss ++ ss',errs) pos' rest
          goStrict2 skipErr (gs,ss,errs) _ [] = (LSLScript "" gs ss,reverse errs)
          goStrict2 skipErr (gs,ss,errs) pos s@(c:cs) =
              case doParse parseStrict2 pos s of
                  Left err -> goPartial2 True (gs,ss,if skipErr then errs else err:errs) pos s
                  Right (PResult rest pos' s) -> goGreedy2 False (gs,ss ++ [s],errs) pos rest
          goPartial2 skipErr (gs,ss,errs) _ [] = (LSLScript "" gs ss,reverse errs)
          goPartial2 skipErr (gs,ss,errs) pos s@(c:cs) =
              case doParse parsePartial pos s of
                  Left err -> goStrict2 True (gs,ss,if skipErr then errs else err:errs) (updatePosChar pos c) cs
                  Right (PResult rest pos' state) -> goResume True state (gs,ss,errs) pos' rest
 
alternateModuleParser srcName string = goStart string
    where doParse p pos = runParser (p pos) newNoAQState srcName
          startPos = initialPos ""
          goStart s =
              case doParse parseStart startPos s of
                  Left err -> goStrict True ([],[],[err]) startPos s
                  Right (PResult rest pos freevars) -> goGreedy False (freevars,[],[]) pos rest
          goGreedy _ (fv,gs,errs) _ [] = (LModule gs fv, errs)
          goGreedy skip (fv,gs,errs) pos s =
              case doParse parseGreedy pos s of
                  Left err -> error ("the greedy parser should never return an error, but returned: " ++ show err)
                  Right (PResult [] _ gs') -> (LModule (gs ++ gs') fv, reverse errs)
                  Right (PResult rest pos' gs') -> goStrict False (fv,gs ++ gs',errs) pos' rest
          goStrict skip (fv,gs,errs) pos [] = (LModule gs fv, reverse errs)
          goStrict skip (fv,gs,errs) pos s@(c:cs) = 
              case doParse parseStrict pos s of
                  Left err -> goStrict True (fv,gs,if skip then errs else err:errs) (updatePosChar pos c) cs
                  Right (PResult rest pos' gs') -> goGreedy False (fv,gs ++ [gs'], errs) pos' rest
          parseGreedy = withRest $ many (try global)
          parseStrict = withRest global
          parseStart = withRest $ do
              whiteSpace
              reserved "$module"
              option [] $ parens params
              
          
-----------------------------------------------------------------------------

parseFromString parser string =
    case runParser parser newNoAQState "" string of
        Left err -> Left (snd (fromParseError err))
        Right x -> Right x

-- | Parse an LSL script into its syntax tree.
parseScriptFromString :: String -> Either String LSLScript
parseScriptFromString s = parseFromString lslParser s
-- | Parse an LSL (Plus) module into its syntax tree.
parseModuleFromString :: String -> Either String LModule
parseModuleFromString s = parseFromString moduleParser s

-- | Parse an LSL script, with possible antiquotations, into its syntax tree.
parseScriptFromStringAQ :: String -> Either ParseError LSLScript
parseScriptFromStringAQ s = runParser lslParser newAQState "" s
-- | Parse an LSL (Plus) module, with possible antiquotations, into its syntax tree.
parseModuleFromStringAQ :: String -> Either ParseError LModule
parseModuleFromStringAQ s = runParser moduleParser newAQState "" s

parseModule :: SourceName -> IO (Either (Maybe SourceContext,String) LModule)
parseModule file = parseFile moduleParser file
parseScript file = parseFile lslParser file

fromParseError :: ParseError -> (Maybe SourceContext,String)
fromParseError err =
        let pos = errorPos err
            msg = showErrorMessages "or" "unknown parse error" 
                                    "expecting" "unexpected" "end of input" (errorMessages err)
        in (Just $ SourceContext TextLocation { textLine0 = sourceLine pos, textColumn0 = sourceColumn pos,
                                                textLine1 = sourceLine pos, textColumn1 = sourceColumn pos,
                                                textName = sourceName pos }
                                 "" "" [],
            msg)

parseFile p file =
    do s <- UTF8.toString <$> B.readFile file
       case parser s of
           Left err -> return $ Left (fromParseError err)
           Right x -> return $ Right x
    where parser = runParser p newNoAQState file
    
pos2Loc (pos0,pos1) pre post = 
     SourceContext TextLocation { 
         textName = sourceName pos0,
         textColumn0 = sourceColumn pos0,
         textLine0 = sourceLine pos0,
         textColumn1 = sourceColumn pos1,
         textLine1 = sourceLine pos1
     } pre post []
     
pos2Ctx (pos0,pos1) pre post pragmas =  
    Just (pos2Loc (pos0,pos1) "" "") { srcPreText = pre, srcPostTxt = post, srcPragmas = pragmas }

combineContexts (Nothing,pos0,pos1,Nothing) = Just $ pos2Loc (pos0,pos1) "" ""
combineContexts (Just (SourceContext (TextLocation l0 c0 l1 c1 n) pre _ prag),_,
                 _,Just (SourceContext (TextLocation l0' c0' l1' c1' n') _ post _ )) =
    Just $ SourceContext (TextLocation l0 c0 l1' c1' n) pre post prag
combineContexts (Just (SourceContext (TextLocation l0 c0 l1 c1 n) pre post prag),_,pos,_) =
    Just $ SourceContext (TextLocation l0 c0 (sourceLine pos) (sourceColumn pos) n) pre post prag
combineContexts (_,pos,_,Just (SourceContext (TextLocation l0 c0 l1 c1 n) pre post prag)) =
    Just $ SourceContext (TextLocation (sourceLine pos) (sourceColumn pos) l1 c1 n) pre post prag

combineContexts1 Nothing _ = Nothing
combineContexts1 _ Nothing = Nothing
combineContexts1 
    (Just (SourceContext (TextLocation l0 c0 _ _ n) pre _ prag))
    (Just (SourceContext (TextLocation _ _ l1 c1 _) _ post _)) = 
        (Just (SourceContext (TextLocation l0 c0 l1 c1 n) pre post prag))
