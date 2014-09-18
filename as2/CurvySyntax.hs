module CurvySyntax
( parseString
, parseFile
, Error (..)
) where

import CurveAST
import SimpleParse
import Control.Applicative (some,(<|>))
import Data.Char (isLetter,isDigit)

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename

data Error = ParseError | StrangeError deriving (Show)

parseString :: String -> Either Error Program
parseString s = case parse program s of
                  (x1,[]):_ -> Right x1
                  (x1,x2):_ -> Left StrangeError
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

program = defs
defs = some def

def :: Parser Def
def = do defWhere <++ defNormal 

defNormal :: Parser Def
defNormal = do i <- ident
               schar '='
               c <- curve
               return $ Def i c []

defWhere :: Parser Def
defWhere = do i <- ident
              schar '='
              c <- curve
              symbol "where"
              schar '{'
              d <- defs
              schar '}'
              return $ Def i c d

curve :: Parser Curve
curve = connect <++
        over <++
        translate <++
        scale <++
        refv <++
        refh <++
        rot <++
        curveR

connect = do c1 <- curveR
             symbol "++"
             c2 <- curve
             return (Connect c1 c2)

over = do c1 <- curveR
          schar '^'
          c2 <- curve
          return (Over c1 c2)

translate = do c1 <- curveR
               symbol "->"
               p <- point
               return (Translate c1 p)

scale = do c1 <- curveR
           symbol "**"
           e <- expr
           return (Scale c1 e)

refv = do c1 <- curveR
          symbol "refv"
          e <- expr
          return (Refv c1 e)

refh = do c1 <- curveR
          symbol "refh"
          e <- expr
          return (Refh c1 e)

rot =  do c1 <- curveR
          symbol "rot"
          e <- expr
          return (Rot c1 e)

curveR :: Parser Curve
curveR = single <|> id' <|> parenthesed

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
           <|> do symbol "height"
                  c <- curve
                  return $ Height c

addOp = do schar '+'
           return (Add)

mulOp = do schar '*'
           return (Mult)

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
