{-# LANGUAGE TupleSections, DeriveFunctor #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.List (span)
import Safe (readMay)

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata go = go . fmap (cata go) . unFix

type Expr = Fix E
data E a = Lit Double
         | Add  a a
         | Sub  a a
         | Mult a a
         | Div  a a
         | Inv  a
         | Pi
         | Ee
         | Exp  a
         | Sin  a
         | Cos  a
         | Tan  a
         | Fact a
         | Log  a
         | Sqrt a
         | Pow  a a
         deriving (Eq, Ord, Show, Functor)

data Tok = Open | Close | Word String
         deriving (Eq, Ord, Show)

-- | Quick and dirty tokenizer. Leaves blank words like @Word ""@ so
-- those ought to be removed before parsing. See 'tokenize'.
tok :: String -> [Tok]
tok []       = []
tok ('(':xs) = Open  : tok xs
tok (')':xs) = Close : tok xs
tok (c  :xs) = tok' c xs [] where
  tok' ' ' xs     acc = Word (reverse acc) : tok xs
  tok' ')' xs     acc = Word (reverse acc) : Close : tok xs
  tok' c   []     acc = [Word (reverse $ c : acc)]
  tok' c   (x:xs) acc = tok' x xs (c:acc)

tokenize :: String -> [Tok]
tokenize = filter (/= Word "") . tok

{-|

  A failing parser type, 'P'. Counts the tokens that were consumed for
debugging purposes. Essentially it's something like @StateT [Tok]
Maybe a@ with the extra "consumed tokens" state outside of the
'Maybe'-induced failure mode.

-}

newtype P a = P { runP :: [Tok] -> Int -> (Maybe (a, [Tok]), Int) }

parse :: P a -> [Tok] -> (Maybe a, Int)
parse p tok = first (fst <$>) $ runP p tok 0

instance Functor P where
  fmap f (P go) = P $ \toks n -> case go toks n of
    (Nothing, n)       -> (Nothing, n)
    (Just (a, toks'), n') -> (Just (f a, toks'), n')

instance Applicative P where
  pure a        = P $ \toks n -> (Just (a, toks), n)
  P ff <*> P xx = P $ \toks n ->
    case ff toks n of
      (Nothing, n)       -> (Nothing, n)
      (Just (f, toks'), n') -> case xx toks' n' of
        (Nothing, n)            -> (Nothing, n)
        (Just (x, toks''), n'') -> (Just (f x, toks''), n'')

instance Alternative P where
  empty         = P $ \_ _ -> (Nothing, 0)
  P p1 <|> P p2 = P $ \toks n -> case p1 toks n of
    (Nothing, _)     -> p2 toks n
    it@(Just res, n) -> it

pRead :: Read a => P a
pRead = P go where
  go (Word x:rest) n = ((,rest) <$> readMay x, succ n)
  go _             n = (Nothing, succ n)

pSatisfy :: (String -> Bool) -> P String
pSatisfy p = P go where
  go (Word x:rest) n =
    if p x then (Just (x, rest), succ n) else (Nothing, succ n)
  go _             n = (Nothing, n)

p :: String -> P String
p s = pSatisfy (==s)

pOpen :: P ()
pOpen = P go where
  go (Open:rest) n = (Just ((), rest), succ n)
  go _           n = (Nothing, succ n)

pClose :: P ()
pClose = P go where
  go (Close:rest) n = (Just ((), rest), succ n)
  go _            n = (Nothing, succ n)

pParens :: P a -> P a
pParens inner = pOpen *> inner <* pClose

oneOf :: [P a] -> P a
oneOf = foldr (<|>) empty

-- | Parses just the expression.
e :: P Expr
e = Fix <$> oneOf
  [ pParens sexpr                   -- nesting

  , Lit <$> pRead                   -- literals
  , Pi  <$  p "pi"
  , Ee  <$  p "e"
  ]
  
sexpr = oneOf
  [ p "+"    *> (Add  <$> e <*> e)  -- applications
  , p "-"    *> (Sub  <$> e <*> e)
  , p "*"    *> (Mult <$> e <*> e)
  , p "/"    *> (Div  <$> e <*> e)
  , p "inv"  *> (Inv  <$> e)
  , p "pow"  *> (Exp  <$> e)
  , p "sin"  *> (Sin  <$> e)
  , p "cos"  *> (Cos  <$> e)
  , p "tan"  *> (Tan  <$> e)
  , p "!"    *> (Fact <$> e)
  , p "ln"   *> (Log  <$> e)
  , p "sqrt" *> (Sqrt <$> e)
  , p "pow"  *> (Pow  <$> e <*> e)
  ]

-- | Fast, memoizing factorial.
fact :: Double -> Double
fact n = fromIntegral $ scanl (*) 1 [1..] !! (round n)

evalF :: E Double -> Double
evalF (Lit  d    )   = d
evalF  Pi            = pi
evalF  Ee            = exp 1
evalF (Add  e1 e2)   = e1 + e2
evalF (Sub  e1 e2)   = e1 - e2
evalF (Mult e1 e2)   = e1 * e2
evalF (Div  e1 e2)   = e1 / e2
evalF (Inv  e    )   = recip (e)
evalF (Exp  e    )   = exp   (e)
evalF (Sin  e    )   = sin   (e)
evalF (Cos  e    )   = cos   (e)
evalF (Tan  e    )   = tan   (e)
evalF (Fact e    )   = fact  (e)
evalF (Log  e    )   = log   (e)
evalF (Sqrt e    )   = sqrt  (e)
evalF (Pow  e1 e2)   = e1 ** e2


{-|

  This calculator evaluates lisplike S-exprs (for simplicity in
parsing). Thus, valid expressions might look like @(+ 1 2)@ or @(inv
100)@. It handles division-by-zero by embedding IEEE 754 floats (as
Haskell 'Double's) which include @NaN@ and both positive and negative
@Infinity@. Unary @'-'@ can precede numbers so long as there isn't a
space: @"-20"@ is ok but @"- 20"@ is likely a parse error.

-}

main :: IO ()
main = forever $ do
  l <- getLine
  case parse e (tokenize l) of
    (Nothing, n)   -> putStrLn $ "No parse (" ++ show n ++ " tokens)"
    (Just expr, _) -> print (cata evalF expr)
