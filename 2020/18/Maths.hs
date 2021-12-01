
import Data.Char

data Token = Op Operator | Operand Int | Bracket [Token] | EOL deriving (Eq, Show)
data Operator = Mult | Add deriving (Eq, Show)
data Expression = T Term | Operation Operator Term Term deriving Show
data Term = V Int | E Expression deriving Show
type FormOperation = (Term -> [Token] -> Expression)

evaluate :: Expression -> Int
evaluate (T t) = evaluateTerm t
evaluate (Operation op l r) = let f = case op of 
                                    Mult -> (*)
                                    Add -> (+) in      
                                        f (evaluateTerm l) (evaluateTerm r)

evaluateTerm :: Term -> Int
evaluateTerm (V v) = v
evaluateTerm (E e) = evaluate e 

toExpression :: FormOperation -> [Token] -> Expression
toExpression fO  (t:[]) = T (toTerm fO t)
toExpression fO (l:ts) = fO (toTerm fO l) ts

toOperation1 :: Term -> [Token] -> Expression
toOperation1 left (Op o:t:ts) = let operation = Operation o left (toTerm toOperation1 t) in
                                if ts == [] 
                                    then operation 
                                    else toOperation1 (E operation) ts                         

toOperation2 :: Term -> [Token] -> Expression
toOperation2 left (Op o:t:[]) = Operation o left (toTerm toOperation2 t)
toOperation2 left (Op o:t:ts) = case o of 
                                    Add -> toOperation2 (E (Operation o left (toTerm toOperation2 t))) ts
                                    Mult -> Operation o left (E (toOperation2 (toTerm toOperation2 t) ts)) 

toTerm :: FormOperation -> Token -> Term
toTerm fO (Bracket b)  = E (toExpression fO b)
toTerm _  (Operand n)  = V n

tokenise :: String -> [Token]
tokenise [] = [] 
tokenise s  = let (t,rem) = nextToken s in (t:tokenise rem)

tokeniseBracket :: [Token] -> String -> ([Token], String)
tokeniseBracket acc (')':rem) = (acc,rem)
tokeniseBracket acc  ss       = let (t, r) = nextToken ss in tokeniseBracket (acc ++ [t]) r

nextToken :: String -> (Token, String)
nextToken []            = (EOL,[])
nextToken (' ':ss)      = nextToken ss
nextToken ('(':ss)      = let (b,rem) = tokeniseBracket [] ss in (Bracket b, rem)
nextToken ('*':ss)      = (Op Mult, ss)
nextToken ('+':ss)      = (Op Add, ss)
nextToken ss            = let (o,rem) = splitWhenFirst (not . isDigit) ss in (Operand ((read :: String -> Int) (takeWhile isDigit ss)),rem) 

splitWhenFirst :: (a -> Bool) -> [a] -> ([a],[a])
splitWhenFirst f xs = splitWhenFirst' f [] xs

splitWhenFirst' :: (a -> Bool) -> [a] -> [a] -> ([a],[a])
splitWhenFirst' _ soFar []     = (soFar, [])
splitWhenFirst' f soFar (x:xs) = if f x then (soFar, (x:xs)) else splitWhenFirst' f (soFar++[x]) xs
