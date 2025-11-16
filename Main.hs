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

letter :: Parser String
letter = string "A" <|> string "B" <|> string "C" <|> string "D"

addBrackets :: String -> String
addBrackets a = "(" ++ a ++ ")"


brackets :: Parser (String -> String)
brackets = Parser $ \input ->
                      Just (addBrackets, input)

expression :: Parser String
expression = (brackets <*> ((++) <$> ((++) <$> disjunct <*> string "->") <*> expression)) <|> disjunct

disjunct :: Parser String
disjunct = (brackets <*> ((++) <$> ((++) <$> conjunct <*> string "|") <*> disjunct)) <|> conjunct

conjunct :: Parser String
conjunct = (brackets <*> ((++) <$> ((++) <$> inversion <*> string "&") <*> conjunct)) <|> inversion

inversion :: Parser String
inversion = letter <|> ((++) <$> ((++) <$> skipString "(" <*> expression) <*> skipString ")") <|> ((++) <$> string "!" <*> inversion)


axiom1 :: Parser String
axiom1 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "(") output
                   (a, rest'') <- runParser disjunct rest'
                   (_, rest''') <- runParser (string "->(") rest''
                   (_, _) <- runParser ((++) <$> disjunct <*> (string ("->" ++ a ++ "))"))) rest'''
                   return (input ++ " (Sch. ax. 1)", rest)

axiom2 :: Parser String
axiom2 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "((") output
                   (a, rest'') <- runParser disjunct rest'
                   (_, rest''') <- runParser (string "->") rest''
                   (b, rest'''') <- runParser disjunct rest'''
                   (_, rest''''') <- runParser (string (")->((" ++ a ++ "->(" ++ b ++ "->")) rest''''
                   (c, rest'''''') <- runParser disjunct rest'''''
                   (_, _) <- runParser (string ("))->(" ++ a ++ "->" ++ c ++ ")))")) rest''''''
                   return (input ++ " (Sch. ax. 2)", rest)

axiom3 :: Parser String
axiom3 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "(") output
                   (a, rest'') <- runParser disjunct rest'
                   (_, rest''') <- runParser (string "->(") rest''
                   (b, rest'''') <- runParser disjunct rest'''
                   (_, _) <- runParser (string ("->(" ++ a ++ "&" ++ b ++ "))")) rest''''
                   return (input ++ " (Sch. ax. 3)", rest) 

axiom4 :: Parser String
axiom4 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "((") output
                   (a, rest'') <- runParser inversion rest'
                   (_, rest''') <- runParser (string "&") rest''
                   (_, rest'''') <- runParser inversion rest'''
                   (_, _) <- runParser (string (")->" ++ a ++ ")")) rest''''
                   return (input ++ " (Sch. ax. 4)", rest)           

axiom5 :: Parser String
axiom5 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "((") output
                   (_, rest'') <- runParser inversion rest'
                   (_, rest''') <- runParser (string "&") rest''
                   (b, rest'''') <- runParser inversion rest'''
                   (_, _) <- runParser (string (")->" ++ b ++ ")")) rest''''
                   return (input ++ " (Sch. ax. 5)", rest)           

axiom6 :: Parser String
axiom6 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "(") output
                   (a, rest'') <- runParser disjunct rest'
                   (_, _) <- runParser ((++) <$> ((++) <$> (string ("->(" ++ a ++ "|")) <*> conjunct) <*> string "))") rest''
                   return (input ++ " (Sch. ax. 6)", rest) 

axiom7 :: Parser String
axiom7 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "(") output
                   (b, rest'') <- runParser disjunct rest'
                   (_, _) <- runParser ((++) <$> ((++) <$> (string "->(") <*> conjunct) <*> string ("|" ++ b ++ "))")) rest''
                   return (input ++ " (Sch. ax. 7)", rest) 

axiom8 :: Parser String
axiom8 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "((") output
                   (a, rest'') <- runParser disjunct rest'
                   (_, rest''') <- runParser (string "->") rest''
                   (c, rest'''') <- runParser disjunct rest'''
                   (_, rest''''') <- runParser (string ")->((") rest''''
                   (b, rest'''''') <- runParser disjunct rest'''''
                   (_, _) <- runParser (string ("->" ++ c ++ ")->((" ++ a ++ "|" ++ b ++ ")->" ++ c ++ ")))")) rest''''''
                   return (input ++ " (Sch. ax. 8)", rest) 

axiom9 :: Parser String
axiom9 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "((") output
                   (a, rest'') <- runParser disjunct rest'
                   (_, rest''') <- runParser (string "->") rest''
                   (b, rest'''') <- runParser disjunct rest'''
                   (_, _) <- runParser (string (")->((" ++ a ++ "->!" ++ b ++ ")->!" ++ a ++ "))")) rest''''
                   return (input ++ " (Sch. ax. 9)", rest)

axiom10 :: Parser String
axiom10 = Parser $ \input -> do
                   (output, rest) <- runParser expression input
                   (_, rest') <- runParser (string "(!!") output
                   (a, rest'') <- runParser inversion rest'
                   (_, _) <- runParser (string ("->" ++ a ++ ")")) rest''
                   return (input ++ " (Sch. ax. 10)", rest)

axiom :: Parser String
axiom = axiom1 <|> axiom2 <|> axiom3 <|> axiom4 <|> axiom5 <|> axiom6 <|> axiom7 <|> axiom8 <|> axiom9 <|> axiom10

main :: IO ()
main = print $ runParser axiom "!!(B->C)->B->C"
