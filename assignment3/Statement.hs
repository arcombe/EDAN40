module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Block [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

if' = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((ex, stx), sty) = If ex stx sty

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

block = accept "begin" -# iter parse #- require "end" >-> buildBlock
buildBlock st = Block st

while = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (ex, st) = While ex st

read' = accept "read" -# word #- require ";" >-> buildRead
buildRead str = Read str

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite ex = Write ex

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment str ex: stmts) dict input = exec stmts (Dictionary.insert (str, Expr.value ex dict) dict) input
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
      then exec (thenStmts: stmts) dict input
      else exec (elseStmts: stmts) dict input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Block stmt : stmts) dict input = exec (stmt ++ stmts) dict input
exec (While cond stmt : stmts) dict input =
    if (Expr.value cond dict) > 0
      then exec (stmt:(While cond stmt):stmts) dict input
      else exec stmts dict input
exec (Read str : stmts) dict (input:inputs) = exec stmts (Dictionary.insert (str, input) dict) inputs
exec (Write ex : stmts) dict input = Expr.value ex dict : exec stmts dict input


instance Parse Statement where
  parse = assignment ! skip ! block ! if' ! while ! read' ! write
  toString = error "Statement.toString not implemented"
