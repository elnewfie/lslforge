module Language.Lsl.Render(renderCompiledScript,renderStatements,renderCtxStatement,renderStatement) where

import Data.List(foldl',intersperse)
import Language.Lsl.Syntax(Expr(..),Func(..),FuncDec(..),Global(..),Handler(..),State(..),Statement(..),
                  Ctx(..),Var(..),LSLType(..),Component(..),ctxItems,CompiledLSLScript(..),
                  SourceContext(..))
import Debug.Trace
tr s x = trace (s ++ show x) x
-- | Generate a string representing an LSL script from a timestamp (string)
-- and a compiled (i.e. validated, with referenced modules included) LSL script.
renderCompiledScript :: String -> CompiledLSLScript -> String
renderCompiledScript stamp (CompiledLSLScript comment globals funcs states) =
   (renderString "// " . renderString stamp . renderString " - LSLForge (0.1.9.1) generated\n" .
    renderString comment .
    renderGlobals globals . renderFuncs funcs . renderStates states . renderString "\n") ""

renderSequence r = (foldl' (.) blank) . (map r)

renderGlobals = renderSequence renderGlobal

renderGlobal (GDecl (Ctx sc var) val) = renderPreText sc . renderVar var .
    case val of
        Nothing -> renderString ";"
        Just expr -> renderString " = " . renderSimple expr . renderString ";"

renderCtxSimple (Ctx _ expr) = renderSimple expr
renderSimple (Neg expr) = renderChar '-' . renderCtxExpr expr
renderSimple (ListExpr l) =
    renderChar '[' .
        (foldl' (.) id $ intersperse (renderChar ',') $ map renderCtxSimple l) .
        renderChar ']'
renderSimple (VecExpr x y z) = renderChar '<' . renderCtxSimple x .
                               renderChar ',' . renderCtxSimple y .
                               renderChar ',' . renderCtxSimple z .
                               renderChar '>'
renderSimple (RotExpr x y z s) = renderChar '<' . renderCtxSimple x .
                                 renderChar ',' . renderCtxSimple y .
                                 renderChar ',' . renderCtxSimple z .
                                 renderChar ',' . renderCtxSimple s .
                                 renderChar '>'
renderSimple e = renderExpression e

renderStates = renderSequence renderState

renderState (Ctx ssc (State (Ctx sc "default") handlers)) =
    renderPreText ssc .
    renderString "default {\n" . renderHandlers handlers . renderString "}"
renderState (Ctx ssc (State (Ctx _ name) handlers)) =
    renderPreText ssc .
    renderString "state " . renderString name . renderString " {\n" . renderHandlers handlers . renderString "}"

renderHandlers = renderSequence renderHandler

renderHandler (Ctx _ (Handler (Ctx sc name) vars stmts)) = renderPreText1 (renderIndent 0) sc . renderHandler' name vars stmts

renderHandler' name vars stmts =
    renderString name . renderChar '(' . renderVarList (ctxItems vars) . renderString ") {\n" .
        renderStatements 1 stmts . renderIndent 0 . renderString "}\n"

renderChar = showChar
renderVar (Var nm t) = (renderType t) . renderChar ' ' . (renderString nm)
renderFuncDec (FuncDec name t vars) =
    let sp = if t == LLVoid then "" else " " in
        renderType t . renderString sp . renderString (ctxItem name) . renderChar '(' .
        renderVarList (ctxItems vars) . renderChar ')'

renderVarList [] = blank
renderVarList (v:vars) =
    (renderVar v) .
        let render' [] = blank
            render' (v:vars) = renderChar ',' . renderVar v . render' vars in render' vars

renderFuncs = renderSequence renderCtxFunc

renderCtxFunc (Ctx sc func) = renderPreText sc . renderFunc func
renderFunc (Func dec stmts) =
    renderFuncDec dec . renderString "{\n" . renderStatements 0 stmts . renderString "}"

renderIndent 0 = renderString "  "
renderIndent n = renderString "  " . renderIndent (n - 1)

renderCtxStatement hang n (Ctx _ s) = renderStatement hang n s

renderStatements n = renderSequence (renderCtxStatement False n)

doHang True n = renderString " "
doHang False n = renderIndent n

renderOptionalExpression Nothing = blank
renderOptionalExpression (Just expr) = renderCtxExpr expr

renderStatement hang n stmt = doHang hang n . renderStatement' n stmt
renderStatement' n (Compound stmts) =
        renderString "{\n" . renderStatements (n+1) stmts . renderIndent n . renderString "}\n"
renderStatement' n (While expr stmt) =
    renderString "while (" . renderCtxExpr expr . renderChar ')' .
    case stmt of
        (Ctx _ NullStmt) -> renderString ";\n"
        _ -> renderCtxStatement True n stmt
renderStatement' n (DoWhile stmt expr) =
    renderString "do " .
    (case stmt of
         (Ctx _ NullStmt) -> renderString ";\n"
         _ -> renderCtxStatement True n stmt) . doHang False n . renderString "while (" . renderCtxExpr expr . renderString ");\n"
renderStatement' n (For mexpr1 mexpr2 mexpr3 stmt) =
    renderString "for (" . renderCtxExprs "" mexpr1 . renderString "; " . renderOptionalExpression mexpr2 .
    renderString "; " . renderCtxExprs "" mexpr3 . renderString ")" .
    case stmt of
        (Ctx _ NullStmt) -> renderString ";\n"
        _ -> renderCtxStatement True n stmt
renderStatement' n (If expr stmt1 stmt2) =
    renderString "if (" . renderCtxExpr expr . renderChar ')' .
        (case stmt1 of
             (Ctx _ NullStmt) -> renderString ";\n"
             (Ctx _ (If expr (Ctx _ (Compound _)) _)) -> renderCtxStatement True n stmt1
             (Ctx _ (If expr _ (Ctx _ NullStmt))) -> case stmt2 of
                 (Ctx _ NullStmt) -> renderCtxStatement True n stmt1
                 _ -> renderStatement True n (Compound [stmt1])
             _ -> renderCtxStatement True n stmt1) .
        case stmt2 of
            (Ctx _ NullStmt) -> blank
            _ -> renderIndent n . renderString "else " . (renderCtxStatement True n stmt2)
renderStatement' n (Decl var val) =
    renderVar var .
        case val of
            Nothing -> renderString ";\n"
            Just expr -> renderString " = " . renderCtxExpr expr . renderString ";\n"
renderStatement' n (NullStmt) = blank . renderString "\n"
renderStatement' n (Return Nothing) = renderString "return;\n"
renderStatement' n (Return (Just expr)) = renderString "return " . renderCtxExpr expr . renderString ";\n";
renderStatement' n (StateChange name) = renderString "state " . renderString name . renderString ";\n";
renderStatement' n (Do expr) = renderCtxExpr expr . renderString ";\n";
renderStatement' n (Label s) = renderChar '@' . renderString s . renderString ";\n";
renderStatement' n (Jump s) = renderString "jump " . renderString s . renderString ";\n";

renderExpressions prefix [] = blank
renderExpressions prefix (e:es) = renderString prefix . renderExpression e . renderExpressions "," es

renderCtxName (Ctx _ n) = renderString n
renderCtxExpr (Ctx _ e) = renderExpression e

renderCtxExprs prefix [] = blank
renderCtxExprs prefix (e:es) = renderString prefix . renderCtxExpr e . renderCtxExprs "," es

renderExpression (IntLit i) = shows i
renderExpression (FloatLit f) = shows f
renderExpression (StringLit s) = renderString ('"':go s)
     where go [] = "\""
           go ('\\':s) = '\\':'\\':go s
           go ('\t':s) = '\\':'t':go s
           go ('\n':s) = '\\':'n':go s
           go ('"':s) = '\\':'"':go s
           go (c:s) = c:go s
renderExpression (KeyLit k) = shows k
renderExpression (VecExpr x y z) =
    renderChar '<' . renderCtxExpr x . renderChar ',' .
                     renderCtxExpr y . renderChar ',' .
                     renderCtxExpr z . renderChar '>'
renderExpression (RotExpr x y z s) =
    renderChar '<' . renderCtxExpr x . renderChar ',' .
                     renderCtxExpr y . renderChar ',' .
                     renderCtxExpr z . renderChar ',' .
                     renderCtxExpr s . renderChar '>'
renderExpression (ListExpr l) =
    let r prefix [] = blank
        r prefix (i:is) = renderString prefix . renderCtxExpr i . r "," is
    in renderChar '[' . r "" l . renderChar ']'
renderExpression (Add expr1 expr2) = renderBinExpr "+" expr1 expr2
renderExpression (Sub expr1 expr2) = renderBinExpr "-" expr1 expr2
renderExpression (Mul expr1 expr2) = renderBinExpr "*" expr1 expr2
renderExpression (Div expr1 expr2) = renderBinExpr "/" expr1 expr2
renderExpression (Mod expr1 expr2) = renderBinExpr "%" expr1 expr2
renderExpression (BAnd expr1 expr2) = renderBinExpr "&" expr1 expr2
renderExpression (Xor expr1 expr2) = renderBinExpr "^" expr1 expr2
renderExpression (BOr expr1 expr2) = renderBinExpr "|" expr1 expr2
renderExpression (Lt expr1 expr2) = renderBinExpr "<" expr1 expr2
renderExpression (Gt expr1 expr2) = renderBinExpr ">" expr1 expr2
renderExpression (Le expr1 expr2) = renderBinExpr "<=" expr1 expr2
renderExpression (Ge expr1 expr2) = renderBinExpr ">=" expr1 expr2
renderExpression (And expr1 expr2) = renderBinExpr "&&" expr1 expr2
renderExpression (Or expr1 expr2) = renderBinExpr "||" expr1 expr2
renderExpression (ShiftL expr1 expr2) = renderBinExpr "<<" expr1 expr2
renderExpression (ShiftR expr1 expr2) = renderBinExpr ">>" expr1 expr2
renderExpression (Inv expr) = renderChar '(' . renderChar '~' . renderCtxExpr expr . renderChar ')'
renderExpression (Not expr) = renderChar '(' . renderChar '!' . renderCtxExpr expr . renderChar ')'
renderExpression (Neg expr) = renderChar '(' . renderChar '-' . renderCtxExpr expr . renderChar ')'
renderExpression (Call name exprs) = renderCtxName name . renderChar '(' . renderCtxExprs "" exprs . renderChar ')'
renderExpression (Cast t expr) = renderString "((" . renderType t . renderChar ')' . renderCtxExpr expr . renderChar ')'
renderExpression (Get var) = renderVarAccess var
--renderExpression (Const var) = renderVarAccess var
--renderExpression (Set var expr) = renderChar '(' . renderVarAccess var . renderString " = " . renderCtxExpr expr . renderChar ')'
renderExpression (Set va expr) = renderAssignment va "=" expr
renderExpression (IncBy va expr) = renderAssignment va "+=" expr
renderExpression (DecBy va expr) = renderAssignment va "-=" expr
renderExpression (MulBy va expr) = renderAssignment va "*=" expr
renderExpression (DivBy va expr) = renderAssignment va "/=" expr
renderExpression (ModBy va expr) = renderAssignment va "%=" expr
renderExpression (Equal expr1 expr2) = renderBinExpr "==" expr1 expr2
renderExpression (NotEqual expr1 expr2) = renderBinExpr "!=" expr1 expr2
renderExpression (PostInc va) = renderInParens (renderVarAccess va . renderString "++")
renderExpression (PostDec va) = renderInParens (renderVarAccess va . renderString "--")
renderExpression (PreInc va) = renderInParens (renderString "++" . renderVarAccess va)
renderExpression (PreDec va) = renderInParens (renderString "--" . renderVarAccess va)

renderInParens f = renderChar '(' . f . renderChar ')'

renderBinExpr op expr1 expr2 = renderChar '(' . renderCtxExpr expr1 . renderChar ' ' .
                               renderString op . renderChar ' ' . renderCtxExpr expr2 . renderChar ')'
renderAssignment va op expr =
    renderChar '(' . renderVarAccess va . renderChar ' ' . renderString op . renderChar ' ' . renderCtxExpr expr . renderChar ')'
renderComponent All = blank
renderComponent X = renderString ".x"
renderComponent Y = renderString ".y"
renderComponent Z = renderString ".z"
renderComponent S = renderString ".s"

renderVarAccess (v,c) = renderCtxName v . renderComponent c

renderString s s' = s ++ s'
renderType LLList = renderString "list"
renderType LLInteger = renderString "integer"
renderType LLVector = renderString "vector"
renderType LLFloat = renderString "float"
renderType LLString = renderString "string"
renderType LLRot = renderString "rotation"
renderType LLKey = renderString "key"
renderType LLVoid = blank

blank :: String -> String
blank = id

renderPreText :: (Maybe SourceContext) -> String -> String
renderPreText = maybe (renderString "\n") (renderString . srcPreText)

renderPreText1 :: (String -> String) -> (Maybe SourceContext) -> String -> String
renderPreText1 f = maybe (renderString "\n" . f) (renderString . srcPreText)
