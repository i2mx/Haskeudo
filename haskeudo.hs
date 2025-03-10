import Data.Array (array)
import Data.Char (isSpace)
import Data.List (isPrefixOf, partition)
import System.Directory.Internal.Prelude (getArgs)
import System.Process (callCommand)
import Text.Parsec
import Text.Parsec.String (Parser)

-- TODO: Add constants

{-
  ┌─────────────────────────────────────────────────────────────────────────┐
  │ Defining Types                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
 -}

type VarName = String

type Program = [String]

type AST = [Stmt]

-- Special things we need for the array type
data Range = Range Int Int
  deriving (Show)

type Dimension = [Range]

data Type
  = IntType
  | BoolType
  | CharType
  | StringType
  | Array Type Dimension
  | UDT String
  deriving (Show)

data Expr
  = Variable VarName
  | IntValue Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | IntDiv Expr Expr
  | Mod Expr Expr
  | Neg Expr
  | BoolLiteral Bool
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Leq Expr Expr
  | Geq Expr Expr
  | StringLiteral String
  | ArrayIndex VarName [Expr]
  | Function VarName [Expr]
  deriving (Show)

-- TODO: I should change the assignment to use a l value and r value parser

data Stmt
  = Declare VarName Type
  | Assign VarName Expr
  | AssignArray VarName [Expr] Expr
  | Cond Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | For VarName Expr Expr Expr [Stmt]
  | Repeat [Stmt] Expr
  | Output [Expr]
  | Input VarName
  | Comment String
  | FunctionDef VarName [(VarName, Type)] Type [Stmt]
  | Return Expr
  deriving (Show)

{-
  ┌─────────────────────────────────────────────────────────────────────────┐
  │ Sample Programs                                                         │
  └─────────────────────────────────────────────────────────────────────────┘
-}

program1 :: [String]
program1 =
  [ "DECLARE x : INTEGER",
    "DECLARE y : INTEGER",
    "x <- 5",
    "y <- 10",
    "IF x < y THEN",
    "  OUTPUT y",
    "ELSE",
    "  OUTPUT x",
    "ENDIF",
    "WHILE x < 10 DO",
    "  x <- x + 1",
    "  OUTPUT x",
    "ENDWHILE"
  ]

ast1 :: [Stmt]
ast1 =
  [ Declare "x" IntType,
    Declare "y" IntType,
    Assign "x" (IntValue 5),
    Assign "y" (IntValue 10),
    Cond
      (Lt (Variable "x") (Variable "y"))
      [Output [Variable "y"]]
      [Output [Variable "x"]],
    While
      (Lt (Variable "x") (IntValue 10))
      [ Assign "x" (Add (Variable "x") (IntValue 1)),
        Output [Variable "x"]
      ]
  ]

{-
  ┌────────────────────────────────────────────────────────────────────────────┐
  │ Compiling AST to C++ Code                                                  │
  └────────────────────────────────────────────────────────────────────────────┘
-}

typeToString :: Type -> String
typeToString IntType = "int"
typeToString BoolType = "bool"
typeToString CharType = "char"
typeToString StringType = "std::string"
typeToString (Array t [Range 1 end]) =
  "std::array<"
    ++ typeToString t
    ++ ","
    ++ show (end + 1)
    ++ ">"
typeToString (Array t (Range 1 end : dims)) =
  "std::array<"
    ++ typeToString (Array t dims)
    ++ ","
    ++ show end
    ++ ">"
typeToString (UDT s) = s

compile :: AST -> String
compile ast = includes ++ declares ++ head ++ body ++ end
  where
    isDeclaration (FunctionDef {}) = True
    isDeclaration _ = False

    includes = "#include <iostream>\n#include <string>\n#include <array>\n\n"
    head = "signed main() {\n"
    (declarations, statements) = partition isDeclaration ast
    declares = unlines $ map compileStmt declarations
    body = unlines $ map compileStmt statements
    end = "return 0; \n}"

-- TODO: add a thingy in front of all variable names to prevent conflicts with c++

compileExpr :: Expr -> String
compileExpr (Variable varName) = varName
compileExpr (IntValue i) = show i
compileExpr (Add e1 e2) = "(" ++ compileExpr e1 ++ " + " ++ compileExpr e2 ++ ")"
compileExpr (Sub e1 e2) = "(" ++ compileExpr e1 ++ " - " ++ compileExpr e2 ++ ")"
compileExpr (Mul e1 e2) = "(" ++ compileExpr e1 ++ " * " ++ compileExpr e2 ++ ")"
compileExpr (Div e1 e2) = "(" ++ compileExpr e1 ++ " / " ++ compileExpr e2 ++ ")"
compileExpr (IntDiv e1 e2) = "(" ++ compileExpr e1 ++ " / " ++ compileExpr e2 ++ ")"
compileExpr (Mod e1 e2) = "(" ++ compileExpr e1 ++ " % " ++ compileExpr e2 ++ ")"
compileExpr (Neg e) = "(-" ++ compileExpr e ++ ")"
compileExpr (BoolLiteral True) = "true"
compileExpr (BoolLiteral False) = "false"
compileExpr (And e1 e2) = "(" ++ compileExpr e1 ++ " && " ++ compileExpr e2 ++ ")"
compileExpr (Or e1 e2) = "(" ++ compileExpr e1 ++ " || " ++ compileExpr e2 ++ ")"
compileExpr (Not e) = "(!" ++ compileExpr e ++ ")"
compileExpr (Eq e1 e2) = "(" ++ compileExpr e1 ++ " == " ++ compileExpr e2 ++ ")"
compileExpr (Neq e1 e2) = "(" ++ compileExpr e1 ++ " != " ++ compileExpr e2 ++ ")"
compileExpr (Lt e1 e2) = "(" ++ compileExpr e1 ++ " < " ++ compileExpr e2 ++ ")"
compileExpr (Gt e1 e2) = "(" ++ compileExpr e1 ++ " > " ++ compileExpr e2 ++ ")"
compileExpr (Leq e1 e2) = "(" ++ compileExpr e1 ++ " <= " ++ compileExpr e2 ++ ")"
compileExpr (Geq e1 e2) = "(" ++ compileExpr e1 ++ " >= " ++ compileExpr e2 ++ ")"
compileExpr (StringLiteral s) = show s
compileExpr (ArrayIndex varName indices) =
  varName
    ++ concatMap (\index -> "[" ++ compileExpr index ++ "]") indices
compileExpr (Function functionName args) =
  functionName
    ++ "("
    ++ combine (fmap compileExpr args)
    ++ ")"
  where
    combine = foldr1 (\x y -> x ++ ", " ++ y)

-- WE ASSUME THAT FUNCTION CALLS CAN ONLY BE USED IN EXPRESSIONS

compileStmt :: Stmt -> String
compileStmt (Declare varName varType) = typeToString varType ++ " " ++ varName ++ ";"
compileStmt (Assign varName exp) = varName ++ " = " ++ compileExpr exp ++ ";"
compileStmt (Cond condExpr thenStmt []) =
  "if ("
    ++ compileExpr condExpr
    ++ ") {\n"
    ++ unlines (map compileStmt thenStmt)
    ++ "}"
compileStmt (Cond condExpr thenStmt elseStmt) =
  "if ("
    ++ compileExpr condExpr
    ++ ") {\n"
    ++ unlines (map compileStmt thenStmt)
    ++ "}\n"
    ++ "else {\n"
    ++ unlines (map compileStmt elseStmt)
    ++ "}"
compileStmt (While condExpr body) =
  "while ("
    ++ compileExpr condExpr
    ++ ") {\n"
    ++ unlines (map compileStmt body)
    ++ "}"
compileStmt (For varName start end (IntValue 1) body) =
  "for ("
    ++ typeToString IntType
    ++ " "
    ++ varName
    ++ " = "
    ++ compileExpr start
    ++ "; "
    ++ varName
    ++ " <= "
    ++ compileExpr end
    ++ "; "
    ++ varName
    ++ "++) {\n"
    ++ unlines (map compileStmt body)
    ++ "}"
compileStmt (For varName start end step body) =
  "for ("
    ++ typeToString IntType
    ++ " "
    ++ varName
    ++ " = "
    ++ compileExpr start
    ++ "; "
    ++ varName
    ++ " <= "
    ++ compileExpr end
    ++ "; "
    ++ varName
    ++ "+= "
    ++ compileExpr step
    ++ ") {\n"
    ++ unlines (map compileStmt body)
    ++ "}"
compileStmt (Output exp) =
  "std::cout << "
    ++ combine (fmap compileExpr exp)
    ++ " << std::endl;"
  where
    combine = foldr1 (\x y -> x ++ " << " ++ y)
compileStmt (Input varName) = "std::cin >> " ++ varName ++ ";"
compileStmt (Repeat body cond) =
  "do {\n"
    ++ unlines (map compileStmt body)
    ++ "} while (!"
    ++ compileExpr cond
    ++ ");"
compileStmt (Comment s) = "// " ++ s
compileStmt (AssignArray varName indices exp) =
  varName
    ++ concatMap (\index -> "[" ++ compileExpr index ++ "]") indices
    ++ " = "
    ++ compileExpr exp
    ++ ";"
compileStmt (FunctionDef functionName args returnType body) =
  typeToString returnType
    ++ " "
    ++ functionName
    ++ "("
    ++ combine (fmap (\(varName, varType) -> typeToString varType ++ " " ++ varName) args)
    ++ ") {\n"
    ++ unlines (map compileStmt body)
    ++ "}"
  where
    combine = foldr1 (\x y -> x ++ ", " ++ y)
compileStmt (Return exp) = "return " ++ compileExpr exp ++ ";"

isComment :: String -> Bool
isComment str = "//" `isPrefixOf` dropWhile isSpace str

formatCode :: [String] -> [String]
formatCode = indent 0
  where
    indent _ [] = []
    indent n (l : ls)
      | isComment l = (replicate (4 * n) ' ' ++ l) : indent n ls
      | l == "" = "" : indent n ls
      | last l == '{' = (replicate (4 * n) ' ' ++ l) : indent (n + 1) ls
      | last l == '}' = (replicate (4 * (n - 1)) ' ' ++ l) : indent (n - 1) ls
      | otherwise = (replicate (4 * n) ' ' ++ l) : indent n ls

{-
  ┌─────────────────────────────────────────────────────────────────────────┐
  │ Parsing Code to AST                                                     │
  └─────────────────────────────────────────────────────────────────────────┘
-}

lexeme :: Parser a -> Parser a
lexeme p = do
  spaces
  x <- p
  spaces
  return x

symbol :: String -> Parser String
symbol s = try $ lexeme (string s)

parens :: Parser a -> Parser a
parens p = do
  _ <- symbol "("
  x <- p
  _ <- symbol ")"
  return x

identifier :: Parser String
identifier = try $ many1 (letter <|> digit <|> char '_')

variable :: Parser Expr
variable = Variable <$> identifier

integer :: Parser Expr
integer = IntValue . read <$> many1 digit

stringLiteral :: Parser Expr
stringLiteral = do
  char '\"'
  str <- many (noneOf "\"")
  char '\"'
  return $ StringLiteral str

boolean :: Parser Expr
boolean = BoolLiteral <$> boolParser
  where
    boolParser = (symbol "TRUE" >> return True) <|> (symbol "FALSE" >> return False)

expr :: Parser Expr
expr = finalExpression
  where
    finalExpression = notExpression `chainl1` andOp `chainl1` orOp
    notExpression = (notOp <*> notExpression) <|> logicalExpression
    logicalExpression = negExpression `chainl1` logicalOp
    negExpression = (negOp <*> negExpression) <|> expression
    expression = factor `chainl1` mulOp `chainl1` addOp `chainl1` modOp
    factor =
      try arrayIndex
        <|> parens expr
        <|> integer
        <|> boolean
        <|> try function
        <|> variable
        <|> stringLiteral

    function = do
      var <- identifier
      symbol "("
      args <- sepBy expr (symbol ",")
      symbol ")"
      return $ Function var args

    arrayIndex = do
      var <- identifier
      symbol "["
      i <- index
      symbol "]"
      return $ ArrayIndex var i

    index = sepBy expr (symbol ",")

    modOp = symbol "%" >> return Mod

    mulOp =
      (symbol "*" >> return Mul)
        <|> (symbol "//" >> return IntDiv)
        <|> (symbol "/" >> return Div)
    addOp =
      (symbol "+" >> return Add)
        <|> (symbol "-" >> return Sub)
    notOp = symbol "NOT" >> return Not

    logicalOp =
      (symbol "<=" >> return Leq)
        <|> (symbol ">=" >> return Geq)
        <|> (symbol "=" >> return Eq)
        <|> (symbol "<>" >> return Neq)
        <|> (symbol "<" >> return Lt)
        <|> (symbol ">" >> return Gt)

    negOp = symbol "-" >> return Neg
    andOp = symbol "AND" >> return And
    orOp = symbol "OR" >> return Or

-- dimensions are of the form [1:10, 2:30,...]

integerLiteral :: Parser Int
integerLiteral = read <$> many1 digit

-- TODO: WE ONLY SUPPORT WHEN THE INDEX STARTS FROM 1 FOR NOW
rangeParser :: Parser Range
rangeParser = do
  start <- integerLiteral
  symbol ":"
  Range start <$> integerLiteral

dimensionsParser :: Parser Dimension
dimensionsParser = rangeParser `sepBy` symbol ","

arrayParser :: Parser Type
arrayParser = do
  symbol "ARRAY"
  symbol "["
  dimensions <- dimensionsParser
  symbol "]"
  symbol "OF"
  Array <$> typeParser <*> pure dimensions

typeParser :: Parser Type
typeParser =
  (symbol "INTEGER" >> return IntType)
    <|> (symbol "BOOLEAN" >> return BoolType)
    <|> (symbol "STRING" >> return StringType)
    <|> (symbol "CHAR" >> return CharType)
    <|> arrayParser
    <|> (UDT <$> identifier)

-- TODO: Multiple statements in one line
declareStmt :: Parser Stmt
declareStmt = do
  symbol "DECLARE"
  var <- identifier
  symbol ":"
  Declare var <$> typeParser

assignStmt :: Parser Stmt
assignStmt = do
  spaces
  var <- identifier
  symbol "<-"
  Assign var <$> expr

outputStmt :: Parser Stmt
outputStmt = do
  symbol "OUTPUT"
  Output <$> sepBy expr (symbol ",")

lValueParser :: Parser String
lValueParser =
  try $
    many1
      ( letter
          <|> digit
          <|> char '_'
          <|> char '.'
          <|> char '['
          <|> char ']'
      )

-- THIS IS SO BAD TODO:
inputStmt :: Parser Stmt
inputStmt = do
  symbol "INPUT"
  Input <$> lValueParser

ifElseStmt :: Parser Stmt
ifElseStmt = do
  symbol "IF"
  cond <- expr
  symbol "THEN"
  thenStmt <- many stmt
  symbol "ELSE"
  elseStmt <- many stmt
  symbol "ENDIF"
  return $ Cond cond thenStmt elseStmt

ifStmt :: Parser Stmt
ifStmt = do
  symbol "IF"
  cond <- expr
  symbol "THEN"
  thenStmt <- many stmt
  symbol "ENDIF"
  return $ Cond cond thenStmt []

condStmt :: Parser Stmt
condStmt = try ifElseStmt <|> ifStmt

whileStmt :: Parser Stmt
whileStmt = do
  symbol "WHILE"
  cond <- expr
  symbol "DO"
  body <- many stmt
  symbol "ENDWHILE"
  return $ While cond body

forOneStmt :: Parser Stmt
forOneStmt = do
  symbol "FOR"
  var <- identifier
  symbol "="
  start <- expr
  symbol "TO"
  end <- expr
  symbol "DO"
  body <- many stmt
  symbol "NEXT"
  symbol var
  return $ For var start end (IntValue 1) body

forStepStmt :: Parser Stmt
forStepStmt = do
  symbol "FOR"
  var <- identifier
  symbol "="
  start <- expr
  symbol "TO"
  end <- expr
  symbol "STEP"
  step <- expr
  symbol "DO"
  body <- many stmt
  symbol "NEXT"
  symbol var
  return $ For var start end step body

forStmt :: Parser Stmt
forStmt = try forStepStmt <|> forOneStmt

repeatStmt :: Parser Stmt
repeatStmt = do
  symbol "REPEAT"
  body <- many stmt
  symbol "UNTIL"
  Repeat body <$> expr

commentStmt :: Parser Stmt
commentStmt = do
  symbol "//"
  Comment <$> many (noneOf "\n")

assignArrayStmt :: Parser Stmt
assignArrayStmt = do
  spaces
  var <- identifier
  symbol "["
  indices <- sepBy expr (symbol ",")
  symbol "]"
  symbol "<-"
  AssignArray var indices <$> expr

arg = do
  spaces
  var <- identifier
  symbol ":"
  dataType <- typeParser
  return (var, dataType)

returnStmt :: Parser Stmt
returnStmt = do
  symbol "RETURN"
  Return <$> expr

functionStmt :: Parser Stmt
functionStmt = do
  symbol "FUNCTION"
  name <- identifier
  symbol "("
  args <- sepBy arg (symbol ",")
  symbol ")"
  symbol "RETURNS"
  returnType <- typeParser
  statements <- many (returnStmt <|> stmt)
  symbol "ENDFUNCTION"
  return $ FunctionDef name args returnType statements

stmt :: Parser Stmt
stmt =
  commentStmt
    <|> forStmt
    <|> whileStmt
    <|> repeatStmt
    <|> try assignStmt
    <|> try assignArrayStmt
    <|> declareStmt
    <|> condStmt
    <|> outputStmt
    <|> inputStmt

program :: Parser [Stmt]
program = many ((functionStmt <|> stmt) <* spaces)

test parser string = print $ parse parser "" string

createExecutable :: String -> String -> String -> IO ()
createExecutable src dest out = do
  source <- readFile src
  case parse program "" source of
    Left err -> print err
    Right ast -> do
      writeFile
        dest
        $ unlines
        $ formatCode
        $ lines
        $ compile ast

      callCommand $ "g++ " ++ dest ++ " -o " ++ out

main :: IO ()
main = do
  args <- getArgs

  case args of
    [src, dest, out] -> createExecutable src dest out
    _ -> putStrLn "Usage: hsc <source> <transpiled destination> <executable destination>"