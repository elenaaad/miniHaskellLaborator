{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use <$>" #-}

import Control.Applicative
import Data.Char

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = []       -- în caz contrar, imposibil de parsat

--- | Acceptă orice caracter
anychar :: Parser Char
anychar = satisfy (\_ -> True)

--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char c = satisfy (\x -> x == c)

--- | acceptă o cifră
digit :: Parser Char
digit = satisfy isDigit

--- | acceptă un spațiu (sau tab, sau sfârșit de linie -- vedeți funcția din Data.Char )
space :: Parser Char
space = satisfy isSpace

--- | succes doar dacă am șirul de intrare este vid 
endOfInput :: Parser ()
endOfInput  = Parser go
  where
    go "" = [((), "")]
    go _ = []

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

parse :: Parser a -> String -> Either String a
parse p s = go (apply (p <* endOfInput) s)
    where 
        go [(x, _)] = Right x
        go _ = Left "Parsare gresita sau ambigua."

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])

cifraSemn :: Parser Int
cifraSemn = do
    semn <- satisfy(\x -> x `elem` "-+")
    char_cifra <- digit
    let cifra = digitToInt char_cifra
    case semn of
        '+' -> return cifra
        '-' -> return (-cifra)


string :: String -> Parser String
string "" = return []
string (c:s) = do
    x <- char c
    y <- string s
    return (x:y)

instance Alternative Parser where
    empty = Parser (const [])
    p <|> p' = Parser (\input -> apply p input ++ apply p' input)

naiveNatural :: Parser Int
naiveNatural = undefined

-- | Elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = do
    _ <- many space
    return ()

-- | parses a natural number (one or more digits)
nat :: Parser Int
nat = string2int <$> some digit

string2int :: String -> Int
string2int s = foldl (\x -> \y -> x*10 + y) 0 (map digitToInt s)

-- | aplică un parser, și elimină spațiile de după
lexeme :: Parser a -> Parser a
-- lexeme p = p <* whiteSpace
lexeme p = do
    r1 <- p
    r2 <- whiteSpace
    return r1 

-- | parses a natural number and skips the space after it
natural :: Parser Int
natural = nat <* whiteSpace

-- | Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
-- symbol s = (string s) <* many space
symbol s = do
    x <- string s
    _ <- whiteSpace
    return x

-- | Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved s = do
    x <- symbol s
    return ()

-- | parsează virgulă, eliminând spațiile de după
comma :: Parser ()
comma = reserved ","

-- | parsează argumentul intre paranteze rotunde
--   elimină spațiile de după paranteze
parens :: Parser a -> Parser a
parens p = reserved "(" *> p <* reserved ")"

-- | parsează argumentul intre paranteze pătrate
--   elimină spațiile de după paranteze
brackets :: Parser a -> Parser a
brackets p = reserved "[" *> p <* reserved "]"

-- | una sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = (:) <$> p <*> some (comma *> p)

-- | zero sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep :: Parser a -> Parser [a]
commaSep p = commaSep1 p <|> pure [] 

-- | date fiind parsere pentru prima literă si pentru felul literelor următoare
--   scrieți un parser pentru un identificator
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter = do
    x <- identStart
    y <- some identLetter
    return (x:y)

-- | ca mai sus, dar elimină spatiile de după
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)

-- | parsează virgulă, eliminând spațiile de după
semi :: Parser ()
semi = reserved ";"

-- | una sau mai multe instanțe, separate de punct-și-virgulă,
--   cu eliminarea spațiilor de după fiecare punct-și-virgulă
--   intoarce lista obiectelor parsate
semiSep1 :: Parser a -> Parser [a]
semiSep1 p
  = do
    a <- p
    as <- many (semi *> p)
    return (a : as)

-- | citește un fișier și aplică analiza sintactică specificată asupra
--    conținutului său
parseFromFile :: Parser a -> FilePath -> IO (Either String a)
parseFromFile parser file
  = do
    str <- readFile file
    case apply parser str of
      [] -> return $ Left "Cannot parse"
      (a,_):_ -> return $ Right a