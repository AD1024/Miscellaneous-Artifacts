module PE where

    import Data.Maybe

    data Val = 
          VInt { runVInt :: Int }
        | VBool { runVBool :: Bool }
        deriving (Show)

    data Expr = 
          Const { runConst :: Val }
        | Var String
        | App String [Expr]
        | Prim Op [Expr]
        | Ite Expr Expr Expr
        deriving (Show)
    
    data Op = Equal | Add | Sub | Mul | Le deriving (Show)

    type Env = [(String, Val)]
    -- fname, (params, exprs)
    type FDef = (String, ([String], Expr))

    type Prog = ([FDef], Expr)

    eval :: Prog -> Val
    eval (fdefs, expr) = eval' expr []
        where
            prim :: Op -> [Val] -> Val
            prim Equal [VInt x, VInt y] = VBool (x == y)
            prim Add   [VInt x, VInt y] = VInt (x + y)
            prim Sub   [VInt x, VInt y] = VInt (x - y)
            prim Mul   [VInt x, VInt y] = VInt (x * y)
            prim Le    [VInt x, VInt y] = VBool (x <= y)
            prim _ _ = error "Runtime Error"


            eval' :: Expr -> Env -> Val
            eval' (Const x) _ = x
            eval' (Var v) env = 
                case lookup v env of
                    Just val -> val
                    Nothing  -> error "undfined variable"
            eval' (App f params) env = 
                eval' body env'
                where
                    (args, body) = fromJust  (lookup f fdefs)
                    env' = zip args [eval' e env | e <- params]
            eval' (Prim op param) env = 
                let rs = [ eval' e env | e <- param ]
                in prim op rs

            eval' (Ite b0 e0 e1) env = 
                if (runVBool $ eval' b0 env) then eval' e0 env
                else eval' e1 env

    prog_fib :: FDef
    prog_fib = (
        "fib",
        (["n"],
            Ite (Prim Le [Var "n", Const (VInt 2)])
                (Const (VInt 1))
                (Prim Add 
                    [(App "fib" [Prim Sub [(Var "n"), (Const (VInt 1))]])
                    ,(App "fib" [Prim Sub [(Var "n"), (Const (VInt 2))]])])))

    test_fib :: Expr
    test_fib = App "fib" [(Const (VInt 10))]

    -- Naive PE

    type EnvPE0 = [(String, Expr)]

    peval :: Prog -> Expr
    peval (fdefs, prog) = peval' prog []
        where

            isVal :: Expr -> Bool
            isVal (Const _) = True
            isVal _         = False

            getVal :: Expr -> Val
            getVal (Const x) = x
            getVal _ = error "Extracting val from non-val terms"

            prim :: Op -> [Val] -> Val
            prim Equal [VInt x, VInt y] = VBool (x == y)
            prim Add   [VInt x, VInt y] = VInt (x + y)
            prim Sub   [VInt x, VInt y] = VInt (x - y)
            prim Mul   [VInt x, VInt y] = VInt (x * y)
            prim Le    [VInt x, VInt y] = VBool (x <= y)
            prim _ _ = error "Runtime Error"

            peval' :: Expr -> EnvPE0 -> Expr
            peval' (Const n) _ = (Const n)
            peval'(Var x) env =
                case (lookup x env) of
                    Just v -> v
                    Nothing -> Var x
            peval' (Prim op exprs) env = 
                let rs = [ peval' e env | e <- exprs ]
                in
                    if all isVal rs then Const (prim op (map getVal rs))
                    else Prim op rs
            peval' (App f es) env = peval' body env'
                where
                    (ss, body) = fromJust (lookup f fdefs)
                    env' = zip ss [ peval' e env | e <- es ]
            
            peval' (Ite b0 e0 e1) env = 
                if isVal cond then
                    if runVBool (getVal cond) then peval' e0 env else peval' e1 env
                else
                    (Ite cond (peval' e0 env) (peval' e1 env))
                    where
                        cond = peval' b0 env

    power :: FDef
    power = ("pow",
        (["x", "n"],
        Ite (Prim Equal [Var "n", Const (VInt 0)])
            (Const (VInt 1))
            (Prim Mul
                [Var "x",
                 App "pow" [Var "x", Prim Sub [Var "n", Const (VInt 1)]]])))
    
    partial_power :: Expr
    partial_power = App "pow" [Var "x", Const (VInt 3)]

    peval_ppower :: Expr
    peval_ppower = peval ([power], partial_power)

    -- Program Specialization
    peval1 :: Prog -> Prog
    type EnvPE1 = [(String, Val)]
