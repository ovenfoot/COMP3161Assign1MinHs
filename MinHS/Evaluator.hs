module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


--TODO: Get rid of the environment by using Elookup then throw to evalSimple
evalE :: VEnv -> Exp -> Value
evalE env (Num n) = I n
evalE env (Con "True") = B True
evalE env (Con "False") = B False
evalE env (Con "Nil") = Nil;
evalE env (App (App (Prim p) e1) (e2)) = evalE env (evalP env (App (App (Prim p) e1) (e2)))
evalE g e = error("G is " ++(show g)++ " e is " ++(show e))
--evalE g e = error "Implement FUCK"

--primops 
evalP :: VEnv -> Exp -> Exp
evalP env (App (App (Prim Add) (Num n)) (Num m)) =  (Num (n+m))
evalP env (App (App (Prim Sub) (Num n)) (Num m)) =  (Num (n-m))
evalP env (App (App (Prim Quot) (Num n)) (Num m)) = (Num (quot n m))
evalP env (App (App (Prim Mul) (Num n)) (Num m)) = (Num (n*m))
evalP env (App (App (Prim Eq) (Num n)) (Num m)) = 
   if n == m then Con "True"
   else Con "False"
evalP env (App (App (Prim Ne) (Num n)) (Num m)) = 
   if n /= m then Con "True"
   else Con "False"
evalP env (App (App (Prim Gt) (Num n)) (Num m)) = 
   if n > m then Con "True"
   else Con "False"
evalP env (App (App (Prim Ge) (Num n)) (Num m)) = 
   if n >= m then Con "True"
   else Con "False"
evalP env (App (App (Prim Lt) (Num n)) (Num m)) = 
   if n < m then Con "True"
   else Con "False"
evalP env (App (App (Prim Le) (Num n)) (Num m)) = 
   if n <= m then Con "True"
   else Con "False"
evalP env (App (App (Prim p) e1) (e2)) = evalP env (App (App (Prim p) (evalP env e1)) (evalP env e2))
evalP g e = error("unimplemented primop case is " ++(show e))


 
