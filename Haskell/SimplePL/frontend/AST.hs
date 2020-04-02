module AST where
    data Literal = IntLit Int | BoolLit Bool deriving (Show, Eq)

    data Expr = 
            Add Expr Expr
        |   Sub Expr Expr
        |   Neg Expr
        |   Mul Expr Expr
        |   Div Expr Expr
        |   Ite Expr Expr Expr
        |   Atom Literal
        deriving (Show, Eq)