module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Prelude

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

{-|
    Part 1
-}

-- | This section of parsers are for Part 1

-- | Parses a character from the alphabet
body :: Parser Char
body = alpha >>= pure

-- | Parses a term from the lambda expression
trm :: Parser Builder
trm = do
    a <- alpha
    pure $ term a

-- | Parses a term from the lambda expression,
-- If it fails, parses another lambda expression or term(s) contained inside round brackets
btrm :: Parser Builder
btrm = trm ||| between (is '(') (is ')') apply

-- | Parses more than one term and concatenates them using the function 'ap'
apply :: Parser Builder
apply = do
    a <- list1 btrm 
    pure $ foldl1 ap a

-- | Parses a long form lambda expression of the Builder type or a set of terms
sel :: Parser Builder
sel = long ||| apply

-- | Parses a character from the English alphabet
alpha :: Parser Char
alpha = oneof "abcdefghijklmnopqrstuvwxyz"

-- | Parses the lambda symbol
lamb :: Parser Char
lamb = is '\955' ||| is '/'

-- | Parses the dot symbol
dot :: Parser Char
dot = is '.'

-- End parsers for Part 1


-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

longLambdaP :: Parser Lambda
longLambdaP = do
    a <- list1 long 
    b <- pure (foldl1 ap a)
    pure $ build b

-- | Parses a long form lambda expression of the Builder type
long :: Parser Builder
long = do
    _ <- is '('
    a <- varl
    b <- sel
    _ <- is ')'
    pure $ lam a b

-- | Parses the start of the lambda expression which consists of the lambda symbol, one character and the dot symbol
varl :: Parser Char
varl = do
    _ <- lamb
    a <- body
    _ <- dot
    pure a

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

shortLambdaP :: Parser Lambda
shortLambdaP = do
    k <- list1 both
    l <- pure $ foldl1 (ap) k
    pure $ build l

-- | Parses both a long form and short form lambda expression of the Builder type
both :: Parser Builder
both = long ||| short

-- | Parses a short form lambda expression of the Builder type
short :: Parser Builder
short = do
    a <- vars
    b <- list1 sel
    c <- pure $foldl1 ap b
    pure $ foldr lam c a

-- | Parses the start of the lambda expression which consists of the lambda symbol, a set of alphabetical characters and the dot symbol
vars :: Parser [Char]
vars = do
    _ <- lamb
    a <- shortBody
    _ <- dot
    pure $ a

-- | Parses the body of a short form lambda expression
shortBody :: Parser [Char]
shortBody = list1 alpha >>= pure

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

{-|
    Part 2
-}

-- | This section of parsers are for Part 2

-- | Function taken from Tim's code stuff https://tgdwyer.github.io/parsercombinators/
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a

-- | Function adapted from Tim's code stuff https://tgdwyer.github.io/parsercombinators/
opr :: [Char] -> Parser [Char]
opr c = do
    _ <- spaces
    _ <- string c
    pure c

-- End parsers for Part 2


-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True
logicP :: Parser Lambda
logicP = do
    a <- chainP
    pure $ build $ a 

-- | Parses a boolean expression contained inside round brackets
boolBrac :: Parser Builder
boolBrac = spaces >> between (is '(') (is ')') chainP

-- | Continously parses boolean expressions
chainP :: Parser Builder
chainP = do
    a <- chain parseAll (parseLogical)
    pure $ a

-- | Parses all types/patterns of boolean expressions
parseAll :: Parser Builder
parseAll = parseIf ||| parseNot ||| parseBoolean ||| boolBrac

-- | Parses the operators 'and' & 'or'
parseLogical :: Parser (Builder -> Builder -> Builder)
parseLogical = parseAnd ||| parseOr

-- | Parses 'True' and 'False'
parseBoolean :: Parser Builder
parseBoolean = parseTrue ||| parseFalse

-- | Parses "True" and returns its church enconding
parseTrue :: Parser Builder
parseTrue = opr "True" >> pure (boolToLam True)

-- | Parses "False" and returns its church encoding
parseFalse :: Parser Builder
parseFalse = opr "False" >> pure (boolToLam False)

-- | Parses "and" and returns an unapplied binary function along with its church encoding
parseAnd :: Parser (Builder -> Builder -> Builder)
parseAnd = opr "and" >> pure (lamAnd)

-- | Returns an unapplied binary function along with the church encoding for "and"
lamAnd :: Builder -> Builder -> Builder
lamAnd a b =  ('x' `lam` 'y' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` term 'y' `ap` (boolToLam False)) `ap` a `ap` b

-- | Parses "or" and returns an unapplied binary function along with its church encoding
parseOr :: Parser (Builder -> Builder -> Builder)
parseOr = opr "or" >> pure (lamOr)

-- | Returns an unapplied binary function along with the church encoding for "or"
lamOr :: Builder -> Builder -> Builder
lamOr a b = ('x' `lam` 'y' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` (boolToLam True) `ap` term 'y') `ap` a `ap` b

-- | Parses "not" and returns its church encoding combined with the boolean expression that comes after it
parseNot :: Parser Builder
parseNot = do
    _ <- opr "not"
    a <- chainP
    l <- pure ('x' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` (boolToLam False) `ap` (boolToLam True))
    pure (l `ap` a)

-- | Parses "if" "then" and "else" combined with the boolean expressions that come between them
parseIf :: Parser Builder
parseIf = do
    _ <- opr "if"
    a <- chainP
    _ <- opr "then"
    b <- chainP
    _ <- opr "else"
    c <- chainP
    l <- pure (lamIf)
    pure (l `ap` a `ap` b `ap` c)

lamIf :: Builder
lamIf = ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f')

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = do
    a <- chain church parseBasic
    pure $ build $ a

-- | Function taken from Week 11 Tutorial
readInt :: String -> Maybe(Int, String)
readInt s = case reads s of 
    [(x, rest)] -> Just (x, rest)
    _           -> Nothing

-- | Function taken from Week 11 Tutorial
parseInt :: Parser Int
parseInt = P f
    where
        f "" = Error UnexpectedEof
        f x = case readInt x of
            Just (v, rest) -> Result rest v
            Nothing        -> Error $ UnexpectedChar (head x)

-- | Parses an integer and converts it into its church encoding
church :: Parser Builder
church = do
    a <- parseInt
    pure $ intToLam a

-- | Parses the "add" and "minus" binary operators
parseBasic :: Parser (Builder -> Builder -> Builder)
parseBasic = parseAdd ||| parseMinus

-- | Parses the "add" binary operator and returns an unapplied binary function along with its church encoding
parseAdd :: Parser (Builder -> Builder -> Builder)
parseAdd = opr "+" >> pure (add)

-- | Returns an unapplied binary function along with the church encoding for the "add" binary operator
add :: Builder -> Builder -> Builder
add x y = ('x' `lam` 'y' `lam` term 'y' `ap` succLam `ap` term 'x') `ap` (x) `ap` (y)

-- | Parses the "minus" binary operator and returns an unapplied binary function along with its church encoding
parseMinus :: Parser (Builder -> Builder -> Builder)
parseMinus = opr "-" >> pure(minus)

-- | Returns an unapplied binary function along with the church encoding for the "minus" binary operator
minus :: Builder -> Builder -> Builder
minus x y = ('x' `lam` 'y' `lam` term 'y' `ap` predLam `ap` term 'x') `ap` (x) `ap` (y)

-- | Church encoding of a function that returns the successor of a given integer
succLam :: Builder
succLam = ('n' `lam` 'f' `lam` 'x' `lam` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))

-- | Church encoding of a function that returns the predecessor of a given integer
predLam :: Builder
predLam = 'n' `lam` 'f' `lam` 'x' `lam` term 'n' `ap` ('g' `lam` 'h' `lam` term 'h' `ap` (term 'g' `ap` term 'f')) `ap` ('u' `lam` term 'x') `ap` ('u' `lam` term 'u')

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
arithmeticP :: Parser Lambda
arithmeticP = do
    a <- parseArith
    pure $ build a

-- | Handles repeated chain of the exponential operator of unknown length
parseComp :: Parser Builder
parseComp = chain expr parseExp

-- | Handles repeated chain of multiplication operator of unknown length
parseExpo :: Parser Builder
parseExpo = chain parseComp parseMultiply

-- | Handles repeated chain of add and minus operators of unknown length
parseArith :: Parser Builder
parseArith = chain parseExpo parseBasic

-- | Parses an integer or an arithmetic expression contained inside round brackets
expr :: Parser Builder
expr = church ||| (spaces >> (between (is '(') (is ')') parseArith)) ||| compIf ||| compNot ||| parseBoolean

-- | Parses the multiplication operator and returns an unapplied binary function along with its church encoding
parseMultiply :: Parser (Builder -> Builder -> Builder)
parseMultiply = opr "*" >> pure (multiply)

-- | Returns an unapplied binary function along with the church encoding for the multiplication operator
multiply :: Builder -> Builder -> Builder
multiply x y = ('x' `lam` 'y' `lam` 'f' `lam` term 'x' `ap` (term 'y' `ap` term 'f')) `ap` x `ap` y

-- | Parses the exponential operator and returns an unapplied binary function along with its church encoding
parseExp :: Parser (Builder -> Builder -> Builder)
parseExp = opr "**" >> pure (expo)

-- | Returns an unapplied binary function along with the church encoding for the exponential operator
expo :: Builder -> Builder -> Builder
expo x y = ('x' `lam` 'y' `lam` term 'y' `ap` term 'x') `ap` x `ap` y


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
complexCalcP :: Parser Lambda
complexCalcP = do
    a <- chain calcBoth parseOthers
    pure $ build a

-- | Parses a complex expression that may be contained inside round brackets
calcBoth :: Parser Builder
calcBoth = calcCompare ||| (spaces >> between (is '(') (is ')') calcCompare)

-- | Handles repeated chain of arithmetic opeartions of unknown length
calcCompare :: Parser Builder
calcCompare = chain parseArith parseOthers

-- | An alternate parser for "if" that works with all kinds of expressions instead of just logical ones
compIf :: Parser Builder
compIf = do
    _ <- opr "if"
    a <- calcBoth
    _ <- opr "then"
    b <- calcBoth
    _ <- opr "else"
    c <- calcBoth
    l <- pure (lamIf)
    pure (l `ap` a `ap` b `ap` c)

-- | An alternate parser for "not" that works with all kinds of expressions instead of just logical ones
compNot :: Parser Builder
compNot = do
    _ <- opr "not"
    a <- calcBoth
    l <- pure ('x' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` (boolToLam False) `ap` (boolToLam True))
    pure (l `ap` a)

-- | Parses either logical or comparison operators
parseOthers :: Parser (Builder -> Builder -> Builder)
parseOthers = parseLogical ||| parseCompare

-- | Parses comparison operators
parseCompare :: Parser (Builder -> Builder -> Builder)
parseCompare = parseMEQ ||| parseLEQ ||| parseEQ ||| parseMore ||| parseNEQ ||| parseLess

-- | Parses the "<=" operator and returns an unapplied binary function along with its church encoding
parseLEQ :: Parser (Builder -> Builder -> Builder)
parseLEQ = opr "<=" >> pure (pLEQ)

-- | Returns an unapplied binary function along with the church encoding for the "<=" operator
pLEQ :: Builder -> Builder -> Builder
pLEQ m n = lamLEQ `ap` m `ap` n

-- | Parses the "==" operator and returns an unapplied binary function along with its church encoding
parseEQ :: Parser (Builder -> Builder -> Builder)
parseEQ = opr "==" >> pure (pEQ)

-- | Returns an unapplied binary function along with the church encoding for the "==" operator
pEQ :: Builder -> Builder -> Builder
pEQ m n = ('m' `lam` 'n' `lam` (varAnd) `ap` (lamLEQ `ap` m `ap` n) `ap` (lamLEQ `ap` n `ap` m)) `ap` m `ap` n

-- | Parses the ">" operator and returns an unapplied binary function along with its church encoding
parseMore :: Parser (Builder -> Builder -> Builder)
parseMore = opr ">" >> pure (pMore)

-- | Returns an unapplied binary function along with the church encoding for the ">" operator
pMore :: Builder -> Builder -> Builder
pMore m n = lamMore `ap` m `ap` n

-- | Parses the "<" operator and returns an unapplied binary function along with its church encoding
parseLess :: Parser (Builder -> Builder -> Builder)
parseLess = opr "<" >> pure (pLess)

-- | Returns an unapplied binary function along with the church encoding for the "<" operator
pLess :: Builder -> Builder -> Builder
pLess m n = lamLess `ap` m `ap` n

-- | Parses the "!=" operator and returns an unapplied binary function along with its church encoding
parseNEQ :: Parser (Builder -> Builder -> Builder)
parseNEQ = opr "!=" >> pure (pNEQ)

-- | Returns an unapplied binary function along with the church encoding for the "!=" operator
pNEQ :: Builder -> Builder -> Builder
pNEQ m n = lamNEQ `ap` m `ap` n

-- | Parses the ">=" operator and returns an unapplied binary function along with its church encoding
parseMEQ :: Parser (Builder -> Builder -> Builder)
parseMEQ = opr ">=" >> pure (pMEQ)

-- | Returns an unapplied binary function along with the church encoding for the ">=" operator
pMEQ :: Builder -> Builder -> Builder
pMEQ m n = lamMEQ `ap` m `ap` n

-- | The church encoding for the isZero function
isZero :: Builder
isZero = 'n' `lam` term 'n' `ap` ('x' `lam` (boolToLam False)) `ap` (boolToLam True)

-- | The church encoding for the "and" operator
varAnd :: Builder
varAnd = ('x' `lam` 'y' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` term 'y' `ap` (boolToLam False))

-- | The church encoding for the "or" operator
varOr :: Builder
varOr = ('x' `lam` 'y' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` (boolToLam True) `ap` term 'y')

-- | The church encoding for the "not" operator
lamNot :: Builder 
lamNot = ('x' `lam` ('b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f') `ap` term 'x' `ap` (boolToLam False) `ap` (boolToLam True))
 
-- | The church encoding for the "minus" operator
varMinus :: Builder
varMinus = ('x' `lam` 'y' `lam` term 'y' `ap` predLam `ap` term 'x')

-- | The church encoding for the "<=" operator
lamLEQ :: Builder
lamLEQ = ('m' `lam` 'n' `lam` isZero `ap` (varMinus `ap` term 'm' `ap` term 'n'))

-- | The church encoding for the "==" operator
lamEQ :: Builder
lamEQ = ('m' `lam` 'n' `lam` varAnd `ap` (lamLEQ `ap` term 'm' `ap` term 'n') `ap` (lamLEQ `ap` term 'n' `ap` term 'm'))

-- | The church encoding for the ">" operator
lamMore :: Builder
lamMore = ('m' `lam` 'n' `lam` lamNot `ap` (lamLEQ `ap` term 'm' `ap` term 'n'))

-- | The church encoding for the "<" operator
lamLess :: Builder
lamLess = ('m' `lam` 'n' `lam` varAnd `ap` (lamLEQ `ap` term 'm' `ap` term 'n') `ap` (lamNEQ `ap` term 'm' `ap` term 'n'))

-- | The church encoding for the "!=" operator
lamNEQ :: Builder
lamNEQ = ('m' `lam` 'n' `lam` lamNot `ap` (lamEQ `ap` term 'm' `ap` term 'n'))

-- | The church encoding for the ">=" operator
lamMEQ :: Builder
lamMEQ = ('m' `lam` 'n' `lam` lamNot `ap` (lamLess `ap` term 'm' `ap` term 'n'))

{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = do
    a <- listAll
    pure $ build a

-- | Parses any pattern of lists
listAll :: Parser Builder
listAll = listNull ||| listSingle ||| listBuild

-- | Parses a list with an unknown number of different values
listBuild :: Parser Builder
listBuild = do
    a <- parseVal
    b <- list1 parseAft
    c <- parseEnd
    pure $ a `ap` (foldl1 ap (b ++ [c]))

-- | Parses a list with a single value
listSingle :: Parser Builder
listSingle = do
    a <- parseVal
    b <- parseEnd
    pure $ a `ap` b

-- | Parses null or an empty list
listNull :: Parser Builder
listNull = do
    a <- parseNull
    pure $ a

-- | Parses the first value inside the list
parseVal :: Parser Builder
parseVal = do
    a <- parseStart
    b <- (church ||| parseBoolean)
    pure $ a `ap` b

-- | Parses a comma and the value after it
parseAft :: Parser Builder
parseAft = do
    a <- parseComma
    b <- (church ||| parseBoolean)
    pure $ a `ap` b

-- | Parses a comma and returns its church encoding
parseComma :: Parser Builder
parseComma = opr "," >> pure (lamCons)

-- | Parses a left bracket and returns the church encoding of Cons
parseStart :: Parser Builder
parseStart = opr "[" >> pure (lamCons)

-- | Parses a right bracket and returns the church encoding of null
parseEnd :: Parser Builder
parseEnd = opr "]" >> pure (lamNull)

-- | Parses null or empty list and returns its church encoding
parseNull :: Parser Builder
parseNull = (opr "null" ||| opr "[]") >> pure (lamNull)

-- | The church encoding of null or empty list
lamNull :: Builder
lamNull = ('c' `lam` 'n' `lam` term 'n')

-- | The church encoding of Cons
lamCons :: Builder
lamCons = ('h' `lam` 't' `lam` 'c' `lam` 'n' `lam` term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n'))

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = do
    a <- list1 parseList
    pure $ build $ foldr1 ap a

-- | Parses any simple list operation
parseList :: Parser Builder
parseList = parseIsNull ||| consList ||| parseHead ||| parseTail ||| listAll 

-- | Parses isNull and returns its church encoding
parseIsNull :: Parser Builder
parseIsNull = opr "isNull" >> pure (lamIsNull)

-- | The church encoding for the function isNull
lamIsNull :: Builder
lamIsNull = ('l' `lam` term 'l' `ap` ('h' `lam` 't' `lam` (boolToLam False)) `ap` (boolToLam True))

-- | Parses cons and returns its church encoding
parseCons :: Parser Builder
parseCons = opr "cons" >> pure (lamCons)

-- | Parses cons and the value to be added to the list
consList :: Parser Builder
consList = do
    a <- parseCons
    b <- (church ||| parseBoolean)
    pure $ a `ap` b

-- | Parses head and returns its church encoding
parseHead :: Parser Builder
parseHead = opr "head" >> pure (lamHead)

-- | The church encoding for the function head
lamHead :: Builder
lamHead = ('l' `lam` term 'l' `ap` ('h' `lam` 't' `lam` term 'h') `ap` (boolToLam False))

-- | Parses tail/rest and returns its church encoding
parseTail :: Parser Builder
parseTail = (opr "tail" ||| opr "rest") >> pure (lamTail)

-- | The church encoding for the function tail/rest
lamTail :: Builder
lamTail = ('l' `lam` 'c' `lam` 'n' `lam` term 'l' `ap` ('h' `lam` 't' `lam` 'g' `lam` term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c')) `ap` ('t' `lam` term 'n') `ap` ('h' `lam` 't' `lam` term 't'))

-- | Exercise 2

-- | Implement your function(s) of choice below!

-- | Parses the function 'fib' and an integer and returns a Fibonacci sequence in lambda calculus

-- | The church encoding for fibonacci sequence is given below (with f being a recursive call of fib and n being the input number)
--- fib n = ycomb (λfn.if n <= 2 then 1 else f(n - 1) + f(n - 2))

-- | The helper functions implemented in previous exercises that were used here are:
--- lamIf: 'if' operator in lambda calculus
--- pLEQ: 'Less than or Equal to' unapplied binary function
--- minus: Unapplied binary function for subtraction
--- plus: Unapplied binary function for addition
--- opr: Parses a given string and spaces that come after it
--- church: Parses an integer and return it in its church encoding

-- >>> lamToInt <$> parse parseFib "fib 0"
-- Result >< Just 0
--
-- >>> lamToInt <$> parse parseFib "fib 1"
-- Result >< Just 1
--
-- >>> lamToInt <$> parse parseFib "fib 2"
-- Result >< Just 1
--
-- >>> lamToInt <$> parse parseFib "fib 6"
-- Result >< Just 8

parseFib :: Parser Lambda
parseFib = fibZero ||| do
    _ <- opr "fib"
    n <- church
    pure $ build $ fib n

-- | Fibonacci sequence list and formula referenced from https://byjus.com/maths/fibonacci-sequence/
-- | Fibonacci formula = Fn = Fn-1 + Fn-2
fib :: Builder -> Builder
fib n = ycomb `ap` ('f' `lam` 'n' `lam` (lamIf `ap` (pLEQ (term 'n') (intToLam 2)) `ap` (intToLam 1) `ap` (add (term 'f' `ap` (minus (term 'n') (intToLam 1))) (term 'f' `ap` (minus (term 'n') (intToLam 2)))))) `ap` n

-- | Parses fib 0 and returns the church encoding of the integer 0
fibZero :: Parser Lambda
fibZero = opr "fib 0" >> pure (build $ intToLam 0)

-- | Church encoding for Y-Combinator referenced from Tim's code stuff https://tgdwyer.github.io/lambdacalculus/
ycomb :: Builder
ycomb = ('f' `lam` ('x' `lam` term 'f' `ap` (term 'x' `ap` term 'x')) `ap` ('x' `lam` term 'f' `ap` (term 'x' `ap` term 'x')))


-- | Parses the a negative number and returns it in lambda calculus 

-- | The church encoding for all the necessary functions are given below along with the references
--- Concept of negative numbers https://en.wikipedia.org/wiki/Church_encoding#Signed_numbers
--- convert = λx.pair x 0
--- negation = λx.pair (second x) (first x)
--- pair = λx.λy.λz.z x y
--- first = λp.p (λx.λy.x)
--- second = λp.p (λx.λy.y)

-- | The helper functions implemented in previous exercises that were used here are:
--- church: Parses an integer and return it in its church encoding

-- >>> parse parseNeg "-1"
-- Result >< Just 0
--
-- >>> parse parseNeg "-2"
-- Result >< Just 1
--
-- >>> parse parseNeg "-10"
-- Result >< Just 1

parseNeg :: Parser Lambda
parseNeg = do
    _ <- is '-'
    a <- church
    pure $ build $ negt `ap` (convert `ap` a)

-- | The church encoding for this section of Builders were referenced from https://en.wikipedia.org/wiki/Church_encoding#Signed_numbers
-- | Lambda calculus of a function that converts a natural number into a signed number
convert :: Builder
convert = ('x' `lam` (lamPair) `ap` term 'x' `ap` (intToLam 0))

-- | Swaps the values in a church pair
negt :: Builder
negt = ('x' `lam` (lamPair) `ap` (lamSecond `ap` term 'x') `ap` (lamFirst `ap` term 'x'))

--- End church encoding for section of Builders


-- | The church encoding for this section of Builders were referenced from https://en.wikipedia.org/wiki/Church_encoding#Church_pairs
-- | Lambda calculus of a function that creates a church pair
lamPair :: Builder
lamPair = ('x' `lam` 'y' `lam` 'z' `lam` term 'z' `ap` term 'x' `ap` term 'y')

-- | Lambda calculus of a function that retrieves the first value in a church pair
lamFirst :: Builder
lamFirst = ('p' `lam` term 'p' `ap` ('x' `lam` 'y' `lam` term 'x'))

-- | Lambda calculus of a function that retrieves the second value in a church pair
lamSecond :: Builder
lamSecond = ('p' `lam` term 'p' `ap` ('x' `lam` 'y' `lam` term 'y'))

--- End church encoding for section of Builders
