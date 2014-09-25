module CurvySyntax
( parseString
, parseFile
, Error (..)
) where

import CurveAST
import SimpleParse
import Control.Applicative ((<|>),(<$>))
import Data.Char (isLetter,isDigit)

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = parseString <$> readFile filename

data Error = SyntaxError String deriving (Show,Eq)

parseString :: String -> Either Error Program
parseString s = case parse program s of
                  (x1,""):_    -> Right x1
                  (_,rest):_   -> Left $ SyntaxError rest
                  []           -> Left $ SyntaxError "Must start with a definition"

digits :: Parser String
digits = munch1 isDigit

number  :: Parser Number
number = token (do pre <- digits
                   char '.'
                   post <- digits
                   return $ read $ pre ++ "." ++ post
                   <|> do i <- digits
                          return $ read i)

-- Not used, because I want to return any errors.
finally :: Parser [Def]
finally = do x <- program
             eof
             return x

program :: Parser [Def]
program = do x <- defs
             more space
             return x

defs :: Parser [Def]
defs = do x <- many1 def
          notFollowedBy def
          return x

def :: Parser Def
def = defWhere <|> defNormal 

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
        over <|>
        translate <|>
        scale <|>
        refv <|>
        refh <|>
        rot <|>
        curveR

connect :: Parser Curve
connect = do c1 <- curveR
             symbol "++"
             c2 <- curve
             return (Connect c1 c2)

over :: Parser Curve
over = do c1 <- curveR
          schar '^'
          c2 <- curve
          return (Over c1 c2)

translate :: Parser Curve
translate = do c1 <- curveR
               symbol "->"
               p <- point
               return (Translate c1 p)

scale :: Parser Curve
scale = do c1 <- curveR
           symbol "**"
           e <- expr
           return (Scale c1 e)

refv :: Parser Curve
refv = do c1 <- curveR
          symbol "refv"
          e <- expr
          return (Refv c1 e)

refh :: Parser Curve
refh = do c1 <- curveR
          symbol "refh"
          e <- expr
          return (Refh c1 e)

rot :: Parser Curve
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

factor :: Parser Expr
factor = do x <- number 
            return $ Con x
         <|> do schar '('
                n <- expr
                schar ')'
                return n

term :: Parser Expr
term = factor `chainl1` mulOp

expr :: Parser Expr
expr = term `chainl1` addOp
       <|> do symbol "width"
              c <- curve
              return $ Width c
       <|> do symbol "height"
              c <- curve
              return $ Height c

addOp :: Parser (Expr -> Expr -> Expr)
addOp = do schar '+'
           return Add

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = do schar '*'
           return Mult

isUnderscore :: Char -> Bool
isUnderscore c = '_'==c

constituent :: Char -> Bool
constituent c = isLetter c || isDigit c || isUnderscore c

keywords :: [String]
keywords = ["where", "refv", "refh", "rot", "width", "height"]

ident :: Parser Ident
ident = do n <- token (munch1 constituent)
           if n `elem` keywords then reject
           else return n
