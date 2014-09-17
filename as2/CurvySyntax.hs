module CurvySyntax
( parseString
, parseFile
, Error (..)
) where

import CurveAST
import SimpleParse
import Control.Applicative

import Data.Char

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

data Error = ParseError deriving (Show)

parseString :: String -> Either Error Program
parseString s = case parse program s of
                  (x1,x2):_ -> Right x1
                  []        -> Left ParseError

digits :: Parser String
digits = munch1 isDigit

number = token number'

number' :: Parser Number
number' = do pre <- digits
             char '.'
             post <- digits
             return $ (read (pre ++ "." ++ post) :: Double)
             <++ do i <- digits
                    return $ read i

addOp = do schar '+'
           return (Add)

mulOp = do schar '*'
           return (Mult)

program = some def

-- argh, left recursive!
def :: Parser Def
def = do defNormal

defNormal :: Parser Def
defNormal = do i <- ident
               schar '='
               c <- curve
               return $ Def i c []

curve :: Parser Curve
curve = single <|> id' <|> parenthesed

parenthesed :: Parser Curve
parenthesed = do schar '('
                 c <- curve
                 schar ')'
                 return c

single :: Parser Curve
single = do x <- point
            return $ Single x 

id' :: Parser Curve
id' = do x <- ident
         return $ Id x 

-- argh! left recursive!!
connect :: Parser Curve
connect = do c1 <- curve
             symbol "++"
             c2 <- curve
             return $ Connect c1 c2


point :: Parser Point
point =  do schar '('
            e1 <- expr
            schar ','
            e2 <- expr
            schar ')'
            return $ Point e1 e2

-- N | (E)
factor :: Parser Expr
factor = do x <- number 
            return $ Con x
         <|> do schar '('
                n <- expr
                schar ')'
                return n

-- factor * factor, factor / factor
-- term :: Parser
-- chainl1 methods are no good. figure out if they can be removed.
term :: Parser Expr
term = factor `chainl1` mulOp

expr :: Parser Expr
expr = do term `chainl1` addOp
       <|> do symbol "width"
              c <- curve
              return $ Width c

isUnderscore :: Char -> Bool
isUnderscore c = '_'==c

isLegal :: Char -> Bool
isLegal c = (isLetter c || isDigit c || isUnderscore c)

keywords :: [String]
keywords = ["where", "refv", "refh", "rot", "width", "height"]

-- letters or digits or underscores
-- not reserved words ??

ident :: Parser Ident
ident = do n <- token (munch1 isLegal)
           if n `elem` keywords then reject
           else return n
