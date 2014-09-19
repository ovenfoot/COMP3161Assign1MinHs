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

evalE env (App (App (Prim p) e1) (e2)) = evalE env (App (App (Prim p) v1) v2) where	
			v1 = devalV(evalE env e1); 
			v2 = devalV(evalE env e2)


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
evalE env (App (Prim Head) e1) = evalE env (App (Prim Head) v1)
	where v1 = devalV(evalE env e1)


--Tail
evalE env (App (Prim Tail) (Con "Nil"))  = error("Cannot retrieve tail from empty List.")
evalE env (App (Prim Tail)(App (App (Con "Cons") (Num n)) (Con "Nil"))) = Cons n Nil
evalE env (App (Prim Tail) (App (App (Con "Cons") (Num n)) e2)) = evalE env (App (Prim Tail) e2) --remove the head
evalE env (App (Prim Tail) (App e1 e2)) = Cons n (evalE env (App (Prim Tail) e2)) where
   I n = evalE env (App (Prim Head) e1)
evalE env (App (Prim Tail) e1) = evalE env (App (Prim Tail) v1)
	where v1 = devalV(evalE env e1)


--Partially implemented primops
evalE env (Prim p) = Close env (Prim p)
evalE env (App (Prim p) (e1)) = Close env (App (Prim p) v1) where
                  v1 = devalV (evalE env e1)



--Letrec
evalE env (Letrec [Bind varname typ [] e1] expFinal) = evalE env' expFinal where
      env' = (E.add (env) (varname,(evalE env e1)))

evalE env (Letrec bs expFinal) = evalE env' (Letrec (init bs) expFinal) where
               Bind varname typ vars e1 = last bs;
               env' = E.add(env) (varname, val);
               val = evalE env e1      
--Letcases (and letfun cases)
--Bind the closure from Letfun into funcname

evalE env (Let [Bind funcname1 _ _ (Letfun b1)] funcapp) = 
  evalE (E.add (env) (funcname1, evalE env (Letfun b1) )) funcapp 


evalE env (Letfun (Bind funcname typ [vars] funcexp)) = Close env' (Letfun b') where
		b' = (Bind funcname typ [vars] funcexp);
		env' = E.add (env) (funcname, Close env (Letfun b'))

evalE env (Letfun (Bind funcname typ (x:xs) funcexp)) = Close env' (Letfun b') where
      b' = (Bind funcname typ (x:xs) funcexp);
      env' = E.add (env) (funcname, Close env (Letfun b'))
   
evalE env (Letfun (Bind funcname typ [] funcexp)) = Close env' (Letfun b') where
    b' = (Bind funcname typ [] funcexp);
    env' = E.add (env) (funcname, Close env (Letfun b'))





{-evalE funcEnv funcbody where 
                                  arg = evalE env exp;
                                   funcEnv = E.addAll (env') [(x, arg), (funcname, Close env' (Letfun (Bind funcname typ (xs) funcbody)))] -}
evalE env (App (Var id) exp) =
  case evalE env (Var id) of 
      Close env' (Letfun (Bind funcname typ [var] funcbody)) -> evalE funcEnv funcbody where 
	                               arg = evalE env exp;
		                             funcEnv = E.addAll (env') [(var, arg), (funcname, Close env' (Letfun (Bind funcname typ [var] funcbody)))]

      Close env' (Letfun (Bind funcname typ (x:xs) funcbody)) -> Close envRed letfunRed  where 
                                  arg = evalE env exp;
                                  letfunRed = (Letfun (Bind funcname typ (xs) funcbody));
                                   envRed = E.addAll (env') [(x, arg), (funcname, Close env' letfunRed )]
   -- Partial primop case
      Close env' (Letfun (Bind funcname typ [] funcbody)) -> evalE env' (App funcbody exp);
    
                            {-evalE funcEnv funcbody where 
                                 arg = evalE env exp;
                                 funcEnv = E.addAll (env') [(funcname, Close env' (Letfun (Bind funcname typ [] funcbody)))] -}

{-
evalE env (App (App (Letfun (Bind funcname1 typ1 [var1] (Letfun (Bind funcname2 typ2 [var2] funcbody)))) e1) e2) = evalE funcenv funcbody where
  funcenv = E.addAll (env) [(var1, evalE env e1 ), (var2, evalE env e2), (funcname1, Close env (Letfun b1)), (funcname2, Close env (Letfun b2))];
  b2 = (Bind funcname2 typ2 [var2] funcbody);
  b1 = (Bind funcname1 typ1 [var1] (Letfun b2));
-}
--evalE g e = error("Unimplented, environment is -->" ++(show g)++ "<-- exp is -->" ++(show e)++"<--")

evalE env (App (Letfun b) e1) =
  case b of   
              (Bind funcname typ [vars] funcexp) -> evalE env'  funcexp where 
                                              val = evalE env e1;
                                              env' = (E.addAll (env) [(vars, val), (funcname, Close env (Letfun b))])

              (Bind funcname typ (x:xs) funcbody) -> Close envRed letfunRed  where 
                                  arg = evalE env e1;
                                  letfunRed = (Letfun (Bind funcname typ (xs) funcbody));
                                   envRed = E.addAll (env) [(x, arg), (funcname, Close env letfunRed )]

              (Bind funcname typ [] funcexp) -> evalE env (App funcexp e1);



evalE env (Let [b] (e2)) = 
   case b of 
      Bind varname typ [] e1 -> evalE (E.add (env) (varname,(evalE env e1))) e2
      Bind varname typ vars e1 -> evalE env' (e2) where
               env' = E.add(env) (varname, Close env (Letfun b));

--evalE g e = error("Unimplented, environment is -->" ++(show g)++ "<-- exp is -->" ++(show e)++"<--")
evalE env (Let (b:bs) expFinal) = evalE env' (Let (bs) expFinal) where
               Bind varname typ vars e1 = b;
               env' = E.add(env) (varname, val);
               val = evalE env e1

{-
evalE env (Let (b:bs) expFinal) = case b of
      Bind varname typ [] e1 -> evalE env' (Let (bs) expFinal) where
               env' = E.add(env) (varname, val);
               val = evalE env e1
      Bind varname typ vars e1 -> evalE env' (Let (bs) expFinal) where
               env' = E.add(env) (varname, Close env (Letfun b));
-}
evalE env (Var id) =
   case E.lookup env id of Just res -> res --error("lookup result is -->"++(show res))
                           Nothing -> error("Error variable not in environment -->" ++ (show id)++"<-- Existing envrionment is -->" ++(show env))


evalE env (App  e1 e2) = 
	case (evalE env e2) of
		Close env2 e2'  -> case (evalE env2 e1) of
							Close env1 e1' -> evalE env1 (App e1' e2')
							_			   -> evalE env2 (App e1 e2')
		_				-> case (evalE env e1) of 
                           (Close env1 e1')  -> evalE env1 (App e1' e2)
							     --_  -> error ("shit--->" ++( show (evalE env e1)))			

{-
evalE env (App  e1 e2) = 
  case (evalE env e1) of
    Close env1 e1'  -> case (evalE env1 e2) of
                        Close env2 e2' -> evalE env2 (App e1' e2')
                        _        -> evalE env1 (App e1' e2)
    _       -> case (evalE env e2) of
                  Close env2 e2' -> evalE env2 (App e1 e2')
                  _        -> evalE env (App v1 v2) where
                    v1 = devalV(evalE env e1);
                    v2 = devalV(evalE env e2);      
-}

--evalE env (App e1 e2) = evalE env (App r1 r2) where 

--Functions





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
devalV (Cons n e1) = App (App (Con "Cons") (Num n)) (devalV e1)
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