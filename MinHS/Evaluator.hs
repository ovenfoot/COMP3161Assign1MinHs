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
--evaluate bs = error("Program text is --->" ++(show bs)++"<---")
evaluate [Bind _ _ _ e] = evalE E.empty e
--evaluate [Bind varname _ n] = evalE E.add (varname n)
evaluate bs = evalE E.empty (Let bs (Var "main"))

--TODO: Get rid of the environment by using Elookup then throw to evalSimple
evalE :: VEnv -> Exp -> Value
--basic end cases
evalE env (Num n) = I n
evalE env (Con "True") = B True
evalE env (Con "False") = B False
evalE env (Con "Nil") = Nil;

--listops
--basic list display
evalE env (App (App (Con "Cons") (Num n)) (Con "Nil")) = Cons n Nil
evalE env (App (App (Con "Cons") (Num n)) e2) = Cons n (evalE env e2)
--head. Collapse list recursively. End when list is one
evalE env (App (Prim Head) (App (Con "Cons") (Num n)))  = I n 
evalE env (App (Prim Head) (Con "Nil"))  = error("Cannot retrieve head from empty List.")
evalE env (App (Prim Head) (App e1 e2)) = evalE env (App (Prim Head) e1)
--Tail
evalE env (App (Prim Tail) (Con "Nil"))  = error("Cannot retrieve tail from empty List.")
evalE env (App (Prim Tail)(App (App (Con "Cons") (Num n)) (Con "Nil"))) = Cons n Nil
evalE env (App (Prim Tail) (App (App (Con "Cons") (Num n)) e2)) = evalE env (App (Prim Tail) e2) --remove the head
evalE env (App (Prim Tail) (App e1 e2)) = Cons (evalInt(evalE env (App (Prim Head) e1))) (evalE env (App (Prim Tail) e2))



--if then else
evalE env (If e1 e2 e3) = evalE env (evalP env (If e1 e2 e3))


--Letcases (and letfun cases)
evalE env (Var id) =
   case E.lookup env id of Just res -> res
                           Nothing -> error("Error not in environment" ++ (show id))

--evalE env (Let [Bind funname _ _ (Letfun (Bind funname _ varname e1))] (App (Var "f") (Num 1))] (e2)) = evalE (E.add (env) (funname,(evalE env b))) e2
evalE env (Let [Bind varname1 _ _ e1] (e2)) = evalE (E.add (env) (varname1,(evalE env e1))) e2

--evalE env (App (Prim p) e1) = evalE env (App (Prim p) (Con (deValue(evalE env e1))))

--evalE env (Let [Bind varname1 _ _ (Num n)] (e1)) = evalE (E.add (env) (varname1,I n)) e1
--evalE env (App (App (Prim p) e1) (e2)) = evalE env (evalP env (App (App (Prim p) e1) (e2)))

--primops
evalE env (App  e1 e2) = evalE env (evalP env (App e1 e2))

--Functions
--evalE env (Letfun (Bind typ x e) = 

--Generic Error
evalE g e = error("Unimplented, environment is -->" ++(show g)++ "<-- exp is -->" ++(show e)++"<--")

--hack listops function
evalInt::Value->Integer
evalInt(I n) = n

--hack listops function
devalV::Value->Exp
--devalV(Nil) = Nil
devalV(I n) = Num n
devalV(Nil) = Con "Nil"
devalV(B True) = Con "True"
devalV(B False) = Con "False"

devalV e = error("devalV not implemented for -->"++(show e)++"<----");

--primops 
evalP :: VEnv -> Exp -> Exp
evalP env (Var id) = 
   case E.lookup env id of Just res -> devalV(res)
                           Nothing -> error("Error not in environment" ++ (show id))
evalP env (Num n) = Num n
evalP env (Con bool) = Con bool
evalP env (App (Prim Neg) (Num n)) = (Num (-n))
evalP env (App (Prim Neg) e1 )= evalP env (App (Prim Neg) (evalP env e1))
evalP env (App (App (Prim Add) (Num n)) (Num m)) =  (Num (n+m))
evalP env (App (App (Prim Sub) (Num n)) (Num m)) =  (Num (n-m))
evalP env (App (App (Prim Quot) (Num n)) (Num m)) = (Num (quot n m))
--evalP env (App (App (Prim Quot) (Num n)) (Num 0)) = error("divide by zero!")
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
--isempty
evalP env (App (Prim Null) (Con "Nil")) = Con "True"
evalP env (App (Prim Null) (Var id)) = evalP env (App (Prim Null) (evalP env (Var id)))
evalP env (App (Prim Null) e1) = Con "False"

evalP env (App (App (Prim p) e1) (e2)) = evalP env (App (App (Prim p) (evalP env e1)) (evalP env e2))
evalP env (If (Con "True") e1 e2) = evalP env e1
evalP env (If (Con "False") e1 e2) = evalP env e2
evalP env (If e1 e2 e3) = evalP env (If (evalP env e1) e2 e3)

evalP g e = error("unimplemented primop case is " ++(show e)++" With enironment "++(show g))
deValue :: Value->[Char]
deValue Nil = "Nil"
 
