module ContMonad where
    import Control.Monad 
    import Control.Monad.Trans.Maybe 
    import Control.Monad.Trans.Class 
    import Control.Monad.Trans.Error
    import Control.Monad.Trans.Identity
    import Control.Monad.Trans.Reader
    import Control.Monad.Trans.Writer
    import Text.Parsec
    import Text.Parsec.String
    import Text.Parsec.Combinator
    import qualified Data.Map as Map
    import Data.Maybe
    import Data.Char
    import Control.Monad.Identity

    -- main = do 
    --     password <- runMaybeT getPassword
    --     case password of 
    --         Just p  -> putStrLn "valid password!"
    --         Nothing -> putStrLn "invalid password!"
    data MilkTea = JustMilkTea | BubbleTea
    data BubbleTea = JustBubbleTea | BubbleTeaWithoutBoba

    isValid :: String -> Bool
    isValid = (>= 10) . length

    getPassword :: MaybeT IO String 
    getPassword = do 
        password <- lift getLine
        guard (isValid password)
        return password 

    type Name = String
    type Env  = Map.Map Name Value

    data Expr = Lit Integer
            |   Abs String Expr
            |   Var Name
            |   App Expr Expr
            |   Plus Expr Expr
        deriving (Show)
    
    data Value = IntVal Integer
            |    FunVal Env Name Expr
        deriving (Show)
    
    infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
    infixOp sep f = string sep >> return f

    expr :: Parser Expr
    expr = do
              exps <- sepBy term (char '+')
              return (foldl1 Plus exps)


    parens :: Parser a -> Parser a
    parens p = do
                satisfy (== '(')
                x <- p
                satisfy (== ')')
                return x

    term :: Parser Expr
    term =  absExpr <|> number <|> appE <|> parens expr

    number :: Parser Expr
    number = do
                n <- many1 (satisfy isDigit)
                return (Lit (read n))

    addE :: Parser Expr
    addE = do
            e1 <- term
            satisfy (== '+')
            e2 <- term
            return (Plus e1 e2)

    appE ::Parser Expr
    appE = do
            fun <- absExpr
            param <- term
            return (App fun param)

    addOp       :: Parser (Expr -> Expr -> Expr)
    addOp = infixOp "+" Plus

    absExpr     :: Parser Expr
    absExpr = do
                char '\\'
                Var param <- ident
                char '.'
                spaces
                body <- expr
                return (Abs param body)

    -- spaces :: Parser String
    -- spaces = many (oneOf " \n\t")

    ident :: Parser Expr
    ident = do
                name <- many (satisfy isAlpha)
                return (Var name)

    eval0 :: Env -> Expr -> Value
    eval0 _ (Lit x) = IntVal x
    eval0 env (Abs name e) = FunVal env name e
    eval0 env (Var name)   = fromJust (Map.lookup name env)
    eval0 env (App e1 e2)  = let
                                fun = eval0 env e1
                                arg = eval0 env e2
                             in
                                case fun of
                                    FunVal env' name expr -> eval0 (Map.insert name arg env') expr
                                    _ -> error "Type mismatch"
    eval0 env (Plus x y)   = let
                                IntVal v1 = eval0 env x
                                IntVal v2 = eval0 env y
                             in
                                IntVal (v1 + v2)


    -- Eval1

    testExpr :: Expr
    testExpr =  (App 
                    (Abs "x" (Plus (Var "x") (Lit 1))) 
                    (Plus (Lit 1) (Lit 2)))

    type Eval1 a = Identity a

    runEval1 :: Eval1 a -> a
    runEval1 ev = runIdentity ev

    eval1 :: Env -> Expr -> Eval1 Value
    eval1 _ (Lit x) = return $ (IntVal x)
    eval1 env (Abs x body) = return $ (FunVal env x body)
    eval1 env (Var x) = return $ (fromJust $ Map.lookup x env)
    eval1 env (App e1 e2) = do
                                funval <- eval1 env e1
                                arg    <- eval1 env e2
                                case funval of
                                    FunVal env' name body -> eval1 (Map.insert name arg env') body
                                    _ -> error "type mismatched"
    eval1 env (Plus x y) = do
                                IntVal j <- eval1 env y
                                IntVal i <- eval1 env x
                                return $ (IntVal $ i + j)
    
    newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

    instance MonadTrans MyMaybeT where
        lift = MyMaybeT . liftM Just
