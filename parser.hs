module Parser where

-- Toy Parser inspired by Stephen Diehl's NanoParsec and Edward Kmett's Trifecta
-- Solomon Bothwell

import Control.Applicative
import Control.Monad
import Control.Monad.Fail

import Data.Char
import Data.Foldable
import Data.Semigroup hiding (option)
import Data.Monoid hiding ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


---------------
---- Types ----
---------------

newtype Parser a = P { runParse :: String -> Maybe (a, String) }


---------------------
---- Typeclasses ----
---------------------

instance Semigroup a => Semigroup (Parser a) where
    (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Parser a) where
    mempty = pure mempty
    mappend = (<>)

instance Functor Parser where
    fmap f p = P $ \x ->
        case runParse p x of
            Nothing -> Nothing
            Just (val, rem) -> Just (f val, rem)

instance Applicative Parser where
    pure v = P $ \x -> Just (v, x)
    pg <*> px = P $ \x ->
        case runParse pg x of
            Nothing -> Nothing
            Just (g, out) -> runParse (fmap g px) out

instance Monad Parser where
    return = pure
    (>>=) pa amb = P $ \x ->
        case runParse pa x of
            Nothing -> Nothing
            Just (a, x') -> runParse (amb a) x'

-- | TODO: Understand why this instance is invalid and correct it:
instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

instance MonadFail Parser where
    fail _ = mzero

instance Alternative Parser where
    empty     = P $ const Nothing
    (<|>) p q = P $ \x ->
        case runParse p x of
            Nothing  -> runParse q x
            res -> res


--------------------------------------
---- Trifecta Parsing Typeclasses ----
--------------------------------------

class Alternative m => Parsing m where
    -- | Take a parser that may consume input, and on failure, go back
    -- | to where we started and fail as if we didn't consume input.
    try           :: m a -> m a
    -- | Give a parser a name | TODO: wat:
    (<?>)         :: m a -> String -> m a
    -- | A version of many that discards its input.
    skipMany      :: m a -> m ()
    -- | skipSome p applies the parser p one or more times, skipping its result.
    skipSome      :: m a -> m ()
    -- | Used to emit an error on an unexpected token.
    unexpected    :: String -> m a
    -- | This parser only succeeds at the end of the input.
    eof           :: m ()
    -- | notFollowedBy p only succeeds when parser p fails.
    -- This parser does not consume any input.
    notFollowedBy :: Show a => m a -> m ()

class CharParsing m where
    -- | Parses if the Char satisfies the predicate.
    satisfy :: (Char -> Bool) -> m Char
    -- | Parses a particular Char only.
    char    :: Char -> m Char
    -- | Parses any char other than `c`.
    notChar :: Char -> m Char
    -- | Parses any single char.
    anyChar :: m Char
    -- | parses a sequence of characters given by s.
    string  :: String -> m String
    -- | parses a sequence of characters determined by the text.
    text    :: Text -> m Text

-- | TODO:
instance Parsing Parser where
    try p         = undefined
    (<?>) p xs    = undefined
    skipMany p    = () <$ many p
    skipSome p    = p *> skipMany p
    unexpected    = undefined
    eof           = undefined
    notFollowedBy = undefined

instance CharParsing Parser where
    satisfy p = P $ uncons >=> (\(y, ys) -> if p y then Just (y, ys) else empty)
    char c    = satisfy (c ==)
    notChar c = satisfy (c /=)
    anyChar   = satisfy $ const True
    string    = traverse char
    text ts   = ts <$ string (T.unpack ts)


---------------------
---- Combinators ----
---------------------

-- | Parses one of a list of Chars
oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- | Dual of `oneOf`. Succeeds if the chart is not in the provided list
noneOf :: String -> Parser Char
noneOf cs = satisfy (not . flip elem cs)

-- | Parses zero or more whitespace characters
spaces :: Parser String
spaces = many space

-- | Parses a whitespace character
space = satisfy isSpace

-- | Parses a newline character
newline :: Parser Char
newline = char '\n'

-- | Parses a tab character
tab :: Parser Char
tab = char '\t'

-- | Parses an uppercase charcter
upper :: Parser Char
upper = satisfy isUpper

-- | Parses a lowercase charcter
lower :: Parser Char
lower = satisfy isLower

-- | Parses a letter or a digit
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

-- | Parses a letter (upper or lower case character)
letter :: Parser Char
letter = upper <|> lower

-- | Parses a digit
digit :: Parser Char
digit = satisfy isDigit

-- | Parses a hexadecimal digit
hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

-- | Parses an octal digit
octDigit :: Parser Char
octDigit = satisfy isOctDigit

-- | Parses a Char is a range
satisfyRange :: Char -> Char -> Parser Char
satisfyRange a z = satisfy (\c -> c >= a && c <= z)

-- | Parses a natural number
natural :: Parser Integer
natural = read <$> some digit

-- | Tokenize a parser
token :: Parser a -> Parser a
token p = do
    res <- p
    spaces
    return res

reserved :: String -> Parser String
reserved = token . string

-- | Parses a integer
number :: Parser Integer
number = do
    s  <- option [] (string "-")
    xs <- some digit
    return $ read (s ++ xs)

parens :: Parser a -> Parser a
parens = between (reserved "(") (reserved ")")

choice :: [Parser a] -> Parser a
choice = asum

option :: a -> Parser a -> Parser a
option a p = p <|> return a

between :: Parser a -> Parser c -> Parser b -> Parser b
between open close p = do
    open
    res <- p
    close
    return res

surroundedBy :: Parser a -> Parser b -> Parser a
surroundedBy p f = between f f p

-- | endBy p sep parses zero or more occurrences of p, separated and ended by sep.
endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

-- | endBy1 p sep parses one or more occurrences of p, separated and ended by sep.
endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 p sep = some (p <* sep)

sepEndBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy p sep = many (p <* optional sep)

sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = some (p <* optional sep)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

count :: Int -> Parser a -> Parser [a]
count = replicateM

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
    where go = ([] <$ end) <|> ((:) <$> p <*> go)


---------------
---- Tests ----
---------------

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Parser a) where
    arbitrary = P <$> arbitrary

instance EqProp a => EqProp (Parser a) where
    (=-=) p p' = property (liftA2 (=-=) (runParse p) (runParse p'))

instance Eq a => EqProp (Sum a) where
    (=-=) a b = property $ getSum a == getSum b

instance Eq a => EqProp (Blind a) where
    (=-=) a b = property $ getBlind a == getBlind b

instance Show (Parser a) where
   show (P a) = "some parser that isn't passing the test"

-- | TODO: Implement `CoArbitary Blind` instances or find workaround
laws = do
    let trigger :: Parser (Sum Int, String, [Sum Int])
        trigger = undefined
    quickBatch $ semigroup (undefined :: Parser (Sum Int))
    quickBatch $ monoid trigger
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
    quickBatch $ monadPlus (undefined :: Parser (String, [Int]))
    quickBatch $ alternative (undefined :: Parser String)
