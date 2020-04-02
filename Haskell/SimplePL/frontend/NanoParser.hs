{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParser where
    import Data.Char
    import Control.Monad
    import Control.Applicative

    -- import AST

    newtype Parser a = Parser { parse :: String -> [(a, String)] }

    runParser :: Parser a -> String -> a
    runParser p s = 
        case parse p s of
            [(res, [])] -> res
            [(_, xs)]   -> error "Did not consume all tokens"
            _           -> error "Parser error"

    instance Functor Parser where
        fmap f (Parser step) = Parser $ \s -> [(f new, xs) | (new, xs) <- step s]

    instance Applicative Parser where
        pure = return
        (Parser cx) <*> (Parser cy) = Parser $ \s -> [(func x, xs) | (func, st1) <- cx s, (x, xs) <- cy st1]
    
    instance Monad Parser where
        return a = Parser $ \s -> [(a, s)]
        p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') (parse p s)
    
    instance MonadPlus Parser where
        mzero = failure
        mplus = combine
    
    instance Alternative Parser where
        empty = mzero
        (<|>)   = option

    item :: Parser Char
    item = Parser $ \s -> 
                case s of
                    []     -> []
                    x : xs -> [(x, xs)]

    failure :: Parser a
    failure = Parser $ \_ -> []

    -- And
    combine :: Parser a -> Parser a -> Parser a
    combine p q = Parser $ \s -> parse p s ++ parse q s

    -- Or
    option  :: Parser a -> Parser a -> Parser a
    option p q = Parser $ \s ->
                            case parse p s of
                                [] -> parse q s
                                res -> res

    some :: Parser a -> Parser [a]
    some v = some_v
                where
                    many_v = some_v <|> pure []
                    some_v = (:) <$> v <*> many_v

    many :: Parser a -> Parser [a]
    many v = many_v
                where
                    many_v = some_v <|> pure []
                    some_v = (:) <$> v <*> many_v
    
    maybe :: Parser a -> Parser (Maybe a)
    maybe p = (Just <$> p) <|> (return Nothing)
    
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = item >>= \c -> if f c then 
                                (return c) 
                                else failure

    chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
    chainl p sep a = (chainl1 p sep) <|> return a

    -- a (sep a)*
    chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
    chainl1 p sep = do  a <- p
                        rest a
                    where
                        rest a = (do
                                    f <- sep
                                    b <- p
                                    rest (f a b)) <|> return a

    parserSeq :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
    parserSeq pa pb f = do
                            car <- pa
                            cdr <- pb
                            return (f car cdr)
    
    parserSeq' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    parserSeq' = liftM2

    char :: Char -> Parser Char
    char c = satisfy (c ==)

    natural :: Parser Integer
    natural = read <$> NanoParser.some (satisfy isDigit)

    string :: String -> Parser String
    string [] = return []
    string (c : cs) = do
                        char c
                        string cs
                        return $ c : cs

    floatString :: Parser String
    floatString = do
                    sign <- NanoParser.maybe (char '-')
                    real <- NanoParser.many (satisfy isDigit) >>= \res -> if res == "" then (return "0") else (return res)
                    satisfy (== '.')
                    mantissa <- NanoParser.some (satisfy isDigit)
                    case sign of
                        Nothing -> return (real ++ "." ++ mantissa)
                        Just _  -> return ("-" ++ real ++ "." ++ mantissa)
                <|> do
                    sign <- NanoParser.maybe (char '-')
                    real <- NanoParser.some (satisfy isDigit)
                    satisfy (== '.')
                    mantissa <- NanoParser.many (satisfy isDigit) >>= \res -> if res == "" then (return "0") else (return res)
                    case sign of
                        Nothing -> return (real ++ "." ++ mantissa)
                        Just _  -> return ("-" ++ real ++ "." ++ mantissa)
                <|> (NanoParser.some (satisfy isDigit))

    scientificString :: Parser String
    scientificString = parserSeq floatString 
                                (NanoParser.maybe $ do
                                        satisfy (== 'e')
                                        sign <- NanoParser.maybe (char '-' <|> char '+')
                                        exp  <- NanoParser.some (satisfy isDigit)
                                        case sign of
                                            Nothing -> return ("e" ++ exp)
                                            Just c  -> return ("e" ++ [ c ] ++ exp))
                                (\num e -> case e of
                                    Nothing -> num
                                    Just x ->  (num ++ x))
    
    float :: Parser Float
    float = scientificString >>= \x -> return (read x)

    appendOne :: Char -> Parser Integer
    appendOne c
        | isDigit c = read <$> (parserSeq' 
                                (\car cdr -> car ++ cdr)
                                (NanoParser.some (satisfy isDigit))
                                (return [ c ]))
        | otherwise = failure

    data Expr = 
            Add Expr Expr
        |   Sub Expr Expr
        |   Mul Expr Expr
        |   Div Expr Expr
        |   Lit Integer
        deriving (Show, Eq)

    eval :: Expr -> Integer
    eval e = case e of
                Add e1 e2 -> eval e1 + eval e2
                Sub e1 e2 -> eval e1 - eval e2
                Mul e1 e2 -> eval e1 * eval e2
                Div e1 e2 -> eval e1 `div` eval e2
                Lit x     -> x


    oneOf :: [Char] -> Parser Char
    oneOf s = satisfy (flip elem s)

    spaces :: Parser String
    spaces = NanoParser.many $ oneOf " \n\r"

    token :: Parser a -> Parser a
    token p = do { spaces; a <- p; spaces ; return a}

    reserved :: String -> Parser String
    reserved s = token (string s)

    parens :: Parser a -> Parser a
    parens m = do
                reserved "("
                n <- m
                reserved ")"
                return n

    infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
    infixOp x f = reserved x >> return f

    prefixOp :: String -> (a -> a) -> Parser (a -> a)
    prefixOp x f = reserved x >> return f

    addop :: Parser (Expr -> Expr -> Expr)
    addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

    multop :: Parser (Expr -> Expr -> Expr)
    multop = infixOp "*" Mul <|> (infixOp "/" Div)

    expr :: Parser Expr
    expr = do
                spaces
                exp <- chainl1 term addop
                spaces
                return (exp)

    term :: Parser Expr
    term = (chainl1 number multop)

    number :: Parser Expr
    number = integer <|> parens expr

    integer :: Parser Expr
    integer = do
                n <- natural
                return (Lit n)

    -- evalIntLit :: Literal -> Int
    -- evalIntLit (IntLit x) = x
    -- evalIntLit _ = error "type mismatched"

    -- evalBoolLit :: Literal -> Bool
    -- evalBoolLit(BoolLit b) = b
    -- evalBoolLit _ = error "type mismatched"

    -- evalE :: Expr -> Expr -> Literal
    -- evalE (Add e1 e2) = IntLit $ (evalIntLit . evalE e1) + (evalIntLit . evalE e2)
    -- evalE (Sub e1 e2) = IntLit $ (evalIntLit . evalE e1) - (evalIntLit . evalE e2)
    -- evalE (Neg e1)    = IntLit $ -(evalIntLit . evalE e1)
    -- evalE (Ite cond ib tb) = if (evalBoolLit (evalE cond)) then evalE e1 else evalE e2