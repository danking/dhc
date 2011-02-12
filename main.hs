-- Durham Haskell Compiler
import qualified Data.Map as M

main =
  return ()

data Expr = Lit Integer
          | Id String
          | Func Expr Expr
          | Apply Expr Expr
          deriving (Show)

type Env = M.Map String Result

data Result = RInt Integer
            | Closure Expr Expr Env
            deriving (Show)

eval :: Expr -> Env -> Maybe Result
eval (Lit x) _ = Just $ RInt x
eval (Id x) env = M.lookup x env
eval (Func a b) env = Just $ Closure a b env
eval (Apply f p) env =
    case eval p env of
      Nothing -> Nothing
      Just param -> case eval f env of
                      Nothing -> Nothing
                      Just (Closure arg body closed_env) ->
                          case arg of
                            Id x -> eval body $ M.insert x param closed_env
                            _ -> Nothing


