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
           | Close VEnv Exp --closure for letfun bindings
           | Vars [Id]
           | Function Exp
           | Letfunname Id
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

evalE :: VEnv -> Exp -> Value
--basic end cases
evalE env (Num n) = I n
evalE env (Con "True") = B True
evalE env (Con "False") = B False
evalE env (Con "Nil") = Nil;

--primops
evalE env (App (Prim Neg) (Num n)) = I (-n)
evalE env (App (Prim Neg) e1 )= evalE env (App (Prim Neg) (Num n)) where
								I n = evalE env e1
evalE env (App (App (Prim Add) (Num n)) (Num m)) =  I (n+m)
evalE env (App (App (Prim Sub) (Num n)) (Num m)) =  I (n-m)
evalE env (App (App (Prim Quot) (Num n)) (Num m)) = I(quot n m)
evalE env (App (App (Prim Mul) (Num n)) (Num m)) = (I(n*m))
evalE env (App (App (Prim Eq) (Num n)) (Num m)) = B (n==m)
evalE env (App (App (Prim Ne) (Num n)) (Num m)) = B (n/=m)
evalE env (App (App (Prim Gt) (Num n)) (Num m)) = B (n>m)
evalE env (App (App (Prim Ge) (Num n)) (Num m)) = B (n>=m)
evalE env (App (App (Prim Lt) (Num n)) (Num m)) = B (n<m)
evalE env (App (App (Prim Le) (Num n)) (Num m)) = B (n<=m)

--isempty
evalE env (App (Prim Null) (Con "Nil")) = B True
evalE env (App (Prim Null) (Var id)) = evalE env (App (Prim Null) v) where v = devalV ((evalE env (Var id)))

evalE env (App (Prim Null) e1) = B False

evalE env (App (App (Prim p) e1) (e2)) = evalE env (App (App (Prim p) (Num n)) (Num m)) where I n = evalE env e1; I m = evalE env e2


--If else
evalE env (If (Con "True") e1 e2) = evalE env e1
evalE env (If (Con "False") e1 e2) = evalE env e2
evalE env (If e1 e2 e3) = evalE env (If tf e2 e3) where
			tf = devalV (evalE env e1)



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
evalE env (App (Prim Tail) (App e1 e2)) = Cons n (evalE env (App (Prim Tail) e2)) where
   I n = evalE env (App (Prim Head) e1)

--evalE g e = error("Unimplented, environment is -->" ++(show g)++ "<-- exp is -->" ++(show e)++"<--")

--Letcases (and letfun cases)
--Bind the closure from Letfun into funcname
evalE env (Let [Bind funcname1 _ _ (Letfun b1)] funcapp) = evalE (E.add (env) (funcname1, evalE env (Letfun b1))) funcapp

evalE env (Letfun (Bind funcname typ [vars] funcexp)) = Close env' (Letfun b') where
		b' = (Bind funcname typ [vars] funcexp);
		env' = env --E.add (env) (funcname, Close env (Letfun b'))


evalE env (App (Letfun (Bind funcname typ [vars] funcexp)) e1) = evalE (E.add (env) (vars, I n)) funcexp where
		I n = evalE env e1

{-evalE env (App (Var funcname) e1) = evalE env (App fun e1) 
		where Close _ fun = evalE env (Var funcname);
-}

evalE env (App (Var id) exp) =  evalE funcEnv funcbody where 
		(Letfun (Bind funcname _ [var] funcbody)) = funcbind;
		Close env' funcbind = evalE env (Var id);
		arg = evalE env exp;
		funcEnv = E.addAll (env') [(var, arg), (funcname, Close env' funcbind)]

evalE env (Let [Bind varname1 _ _ e1] (e2)) = evalE (E.add (env) (varname1,(evalE env e1))) e2
  
evalE env (Var id) =
   case E.lookup env id of Just res -> res--error("lookup result is -->"++(show res))
                           Nothing -> error("Error variable not in environment -->" ++ (show id)++"<-- Existing envrionment is -->" ++(show env))
                           
           
--Recursion
--evalE env (App (Var id1) (Var id2)) = error("foundsomething")
--evalE env (App  e1 e2) = error("starting recursion on -->" ++(show e1)++"<--- and -->"++(show e2))
--evalE env (App  e1 e2) = evalE env (App (devalV (evalE env e1)) (devalV (evalE env e2)))

evalE env (App  e1 e2) = 
	case (evalE env e1) of
		Close env' e1'  -> evalE env' (App e1' e2)	
		_				-> error("SHIT NIGGA")
--Functions
--evalE env (Letfun (Bind typ x e) = 

--Generic Error
evalE g e = error("Unimplented, environment is -->" ++(show g)++ "<-- exp is -->" ++(show e)++"<--")

--hack listops function
devalV::Value->Exp
--devalV(Nil) = Nil
devalV(I n) = Num n
devalV(Nil) = Con "Nil"
devalV(B True) = Con "True"
devalV(B False) = Con "False" 
--devalV (Close env exp) = exp
devalV e = error("devalV not implemented for -->"++(show e)++"<----");


{-
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
-}