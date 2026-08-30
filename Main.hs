module Main where

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where {
fmap f (Parser p) = Parser $ \input -> case (p input) of
                                       Nothing -> Nothing
                                       Just (output, rest) -> Just (f output, rest)
}

instance Applicative Parser where {
pure a = Parser $ \input -> Just (a, input);
Parser f <*> Parser p = Parser $ \input -> case (f input) of
                                           Nothing -> Nothing
                                           Just (f', rest) -> case (p rest) of
                                                                  Nothing -> Nothing
                                                                  Just (output, rest') -> Just (f' output, rest')
}

empty :: Parser a
empty = Parser $ \_ -> Nothing

(<|>) :: Parser a -> Parser a -> Parser a
Parser f <|> Parser p = Parser $ \input -> case (f input) of
                                           Just (output, rest) -> Just (output, rest)
                                           Nothing -> p input

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
                              case input of
                              "" -> Nothing
                              a:rest -> case predicate a of
                                        False -> Nothing
                                        True -> Just (a, rest)

char :: Char -> Parser Char
char i = satisfy (==i)

string :: String -> Parser String
string "" = pure ""
string (x:xs) = (:) <$> char x <*> string xs

skip :: (Char -> Bool) -> Parser String
skip predicate = Parser $ \input ->
                              case input of
                              "" -> Nothing
                              a:rest -> case predicate a of
                                        False -> Nothing
                                        True -> Just ("", rest)

skipChar :: Char -> Parser String
skipChar i = skip (==i)

skipString :: String -> Parser String
skipString "" = pure ""
skipString (x:xs) = (++) <$> skipChar x <*> skipString xs

failed :: Parser Char
failed = Parser $ \_ -> Nothing

letter :: Parser Char
letter = foldl (<|>) failed [char x | x <- ['A'..'Z']]

digit :: Parser Char
digit = foldl (<|>) failed [char x | x <- ['0'..'9']]

variable' :: Parser String
variable' = ((:) <$> (digit <|> letter) <*> variable') <|> string ""

data Expr = Disjunct Expr Expr | Conjunct Expr Expr | Implic Expr Expr | Negation Expr | Variable String
            deriving (Show, Eq)

variable :: Parser Expr
variable = Variable <$> ((:) <$> letter <*> variable')

impOp :: Parser (Expr -> Expr -> Expr)
impOp = (\_ -> Implic) <$> string "->"

disOp :: Parser (Expr -> Expr -> Expr)
disOp = (\_ -> Disjunct) <$> string "|"

conOp :: Parser (Expr -> Expr -> Expr)
conOp = (\_ -> Conjunct) <$> string "&"

negOp :: Parser (Expr -> Expr)
negOp = (\_ -> Negation) <$> string "!"

comb2 :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
comb2 x y z = (\a op b -> op a b) <$> x <*> y <*> z

comb1 :: Parser (Expr -> Expr) -> Parser Expr -> Parser Expr
comb1 x y = (\op a -> op a) <$> x <*> y

expression :: Parser Expr
expression = (comb2 disjunct impOp expression) <|> disjunct

disjunct :: Parser Expr
disjunct = (comb2 conjunct disOp disjunct) <|> conjunct

conjunct :: Parser Expr
conjunct = (comb2 negat conOp conjunct) <|> negat

negat :: Parser Expr
negat = variable <|> ((\_ y _ -> y) <$> skipChar '(' <*> expression <*> skipChar ')') <|> (comb1 negOp negat)

mp :: Expr -> Expr -> Expr -> Bool
mp b a (Implic a0 b0) = a == a0 && b == b0
mp _ _ _ = False

ax1 :: Expr -> String
ax1 prop = case prop of
           (Implic a0 (Implic _ a1)) -> if a0 == a1 then "Axioms scheme 1" else ax2 prop
           _ -> ax2 prop 

ax2 :: Expr -> String
ax2 prop = case prop of
           (Implic (Implic a0 b0) (Implic (Implic a1 (Implic b1 c1)) (Implic a2 c2))) -> if a0 == a1 && a1 == a2 && b0 == b1 && c1 == c2 then "Axioms scheme 2" else ax3 prop
           _ -> ax3 prop

ax3 :: Expr -> String
ax3 prop = case prop of
           (Implic a0 (Implic b0 (Conjunct a1 b1))) -> if a0 == a1 && b0 == b1 then "Axioms scheme 3" else ax4 prop
           _ -> ax4 prop

ax4 :: Expr -> String
ax4 prop = case prop of
           (Implic (Conjunct a0 _) a1) -> if a0 == a1 then "Axioms scheme 4" else ax5 prop
           _ -> ax5 prop

ax5 :: Expr -> String
ax5 prop = case prop of
           (Implic (Conjunct _ b0) b1) -> if b0 == b1 then "Axioms scheme 5" else ax6 prop
           _ -> ax6 prop

ax6 :: Expr -> String
ax6 prop = case prop of
           (Implic a0 (Disjunct a1 _)) -> if a0 == a1 then "Axioms scheme 6" else ax7 prop
           _ -> ax7 prop

ax7 :: Expr -> String
ax7 prop = case prop of
           (Implic b0 (Disjunct _ b1)) -> if b0 == b1 then "Axioms scheme 7" else ax8 prop
           _ -> ax8 prop

ax8 :: Expr -> String
ax8 prop = case prop of
           (Implic (Implic a0 c0) (Implic (Implic b1 c1) (Implic (Disjunct a2 b2) c2))) -> if a0 == a2 && b1 == b2 && c0 == c1 && c1 == c2 then "Axioms scheme 8" else ax9 prop
           _ -> ax9 prop

ax9 :: Expr -> String
ax9 prop = case prop of
           (Implic (Implic a0 b0) (Implic (Implic a1 (Negation b1)) (Negation a2))) -> if a0 == a1 && a1 == a2 && b0 == b1 then "Axioms scheme 9" else ax10 prop
           _ -> ax10 prop

ax10 :: Expr -> String
ax10 prop = case prop of
            (Implic (Negation (Negation a0)) a1) -> if a0 == a1 then "Axioms scheme 10" else "Not proven"
            _ -> "Not proven"

main :: IO ()
-- main = print $ runParser expression "(A->A->A)"
main = print $ ax1 (Implic (Implic (Variable "A") (Implic (Variable "A") (Variable "A"))) (Implic (Implic (Variable "A") (Implic (Implic (Variable "A") (Variable "A")) (Variable "A"))) (Implic (Variable "A") (Variable "A"))))