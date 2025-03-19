import Data.Char (isSpace)
import Data.List (isPrefixOf, partition)
import System.Directory.Internal.Prelude (getArgs)
import System.Process (callCommand)
import Text.Parsec
import Text.Parsec.String (Parser)

-- TODO: Add constants
-- TODO: Add record types

{-
  ┌─────────────────────────────────────────────────────────────────────────┐
  │ Defining Types                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
 -}

type VarName = String

-- type Program = [String]
type AST = [Stmt]

-- Special things we need for the array type
data Range = Range Int Int
  deriving (Show)

type Dimension = [Range]

data Type
  = IntType
  | RealType
  | BoolType
  | CharType
  | StringType
  | Array Type Dimension
  | UDT String
  deriving (Show)

data Expr
  = Variable VarName
  | IntValue Int
  | RealValue Float
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
  | Constant VarName Expr
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
  | ProcedureDef VarName [(VarName, Type)] [Stmt]
  | Call VarName [Expr]
  | Return Expr
  deriving (Show)

{-
  ┌─────────────────────────────────────────────────────────────────────────┐
  │ Sample Programs                                                         │
  └─────────────────────────────────────────────────────────────────────────┘
-}

-- program1 :: Program
-- program1 =
--   [ "DECLARE x : INTEGER",
--     "DECLARE y : INTEGER",
--     "x <- 5",
--     "y <- 10",
--     "IF x < y THEN",
--     "  OUTPUT y",
--     "ELSE",
--     "  OUTPUT x",
--     "ENDIF",
--     "WHILE x < 10 DO",
--     "  x <- x + 1",
--     "  OUTPUT x",
--     "ENDWHILE"
--   ]

-- ast1 :: [Stmt]
-- ast1 =
--   [ Declare "x" IntType,
--     Declare "y" IntType,
--     Assign "x" (IntValue 5),
--     Assign "y" (IntValue 10),
--     Cond
--       (Lt (Variable "x") (Variable "y"))
--       [Output [Variable "y"]]
--       [Output [Variable "x"]],
--     While
--       (Lt (Variable "x") (IntValue 10))
--       [ Assign "x" (Add (Variable "x") (IntValue 1)),
--         Output [Variable "x"]
--       ]
--   ]

{-
  ┌────────────────────────────────────────────────────────────────────────────┐
  │ Compiling AST to C++ Code                                                  │
  └────────────────────────────────────────────────────────────────────────────┘
-}

typeToString :: Type -> String
typeToString IntType = "int"
typeToString BoolType = "bool"
typeToString CharType = "char"
typeToString RealType = "float"
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
typeToString (Array _ _) = error "Invalid array dimensions"
typeToString (UDT s) = s

combine :: [String] -> String
combine [] = ""
combine s = foldr1 (\x y -> x ++ ", " ++ y) s

compile :: AST -> String
compile ast = includes ++ declares ++ header ++ body ++ end
  where
    isDeclaration (FunctionDef {}) = True
    isDeclaration (ProcedureDef {}) = True
    isDeclaration _ = False

    includes = "#include <random> \n#include <string> \n#include <iostream> \n#include <random> \n#include <sstream> \n#include <cctype> \n#include <array> \n\nint INT(const float& num1) { \n    return (int)num1; \n} \n\nfloat RANDOM() { \n    return std::rand(); \n} \n\n// STRING FUNCTIONS \n\nstd::string MID(const std::string& ThisString, int x, int y) { \n    return ThisString.substr(x, x + y - 1); \n} \n\nint LENGTH(const std::string& ThisString) { \n    return ThisString.length(); \n} \n\nstd::string SUBSTRING(const std::string& ThisString, int start, int end) { \n    return ThisString.substr(start, end); \n} \n\nstd::string LEFT(const std::string& ThisString, int x) { \n    return ThisString.substr(0, x); \n} \n\nstd::string RIGHT(const std::string& ThisString, int x) { \n    return ThisString.substr(ThisString.length() - x, x); \n} \n\nchar LCASE(char ThisChar) { \n    return std::tolower(ThisChar); \n} \n\nchar UCASE(char ThisChar) { \n    return std::toupper(ThisChar); \n} \n\nstd::string TO_UPPER(const std::string& ThisString) { \n    std::string result = ThisString; \n    for (char& c : result) { \n        c = std::toupper(c); \n    } \n    return result; \n} \n\nstd::string TO_LOWER(const std::string& ThisString) { \n    std::string result = ThisString; \n    for (char& c : result) { \n        c = std::tolower(c); \n    } \n    return result; \n} \n\nstd::string NUM_TO_STRING(double x) { \n    std::ostringstream oss; \n    oss << x; \n    return oss.str(); \n} \n\ndouble STRING_TO_NUM(const std::string& x) { \n    return std::stod(x); \n} \n\nint ASC(char ThisChar) { \n    return static_cast<int>(ThisChar); \n} \n\nchar CHR(int x) { \n    return static_cast<char>(x); \n}\n\n"
    header = "signed main() {\n"
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
compileExpr (RealValue r) = show r

-- WE ASSUME THAT FUNCTION CALLS CAN ONLY BE USED IN EXPRESSIONS

compileStmt :: Stmt -> String
compileStmt (Declare varName varType) = typeToString varType ++ " " ++ varName ++ ";"
compileStmt (Assign varName expression) = varName ++ " = " ++ compileExpr expression ++ ";"
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
compileStmt (Output expression) =
  "std::cout << "
    ++ combineOut (fmap compileExpr expression)
    ++ " << std::endl;"
  where
    combineOut = foldr1 (\x y -> x ++ " << " ++ y)
compileStmt (Input varName) = "std::cin >> " ++ varName ++ ";"
compileStmt (Repeat body cond) =
  "do {\n"
    ++ unlines (map compileStmt body)
    ++ "}\n"
    ++ "while (!"
    ++ compileExpr cond
    ++ ");"
compileStmt (Comment s) = "// " ++ s
compileStmt (AssignArray varName indices expression) =
  varName
    ++ concatMap (\index -> "[" ++ compileExpr index ++ "]") indices
    ++ " = "
    ++ compileExpr expression
    ++ ";"
compileStmt (FunctionDef functionName args returnType body) =
  typeToString returnType
    ++ " "
    ++ functionName
    ++ "("
    ++ combine (fmap (\(varName, varType) -> typeToString varType ++ " " ++ varName) args)
    ++ ") {\n"
    ++ unlines (map compileStmt body)
    ++ "}\n"
compileStmt (Return expression) = "return " ++ compileExpr expression ++ ";"
compileStmt (ProcedureDef procedureName args body) =
  "void "
    ++ procedureName
    ++ "("
    ++ combine (fmap (\(varName, varType) -> typeToString varType ++ " " ++ varName) args)
    ++ ") {\n"
    ++ unlines (map compileStmt body)
    ++ "}\n"
compileStmt (Call functionName args) =
  "(void) "
    ++ functionName
    ++ "("
    ++ combine (fmap compileExpr args)
    ++ ");"
compileStmt (Constant varName value) =
  "const "
    ++ typeName value
    ++ " "
    ++ varName
    ++ " = "
    ++ compileExpr value
    ++ ";"
  where
    typeName :: Expr -> String
    typeName (IntValue _) = "int"
    typeName (RealValue _) = "float"
    typeName (BoolLiteral _) = "bool"
    typeName (StringLiteral _) = "std::string"

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

padding :: Parser ()
padding = skipMany $ oneOf " \t"

nextLine :: Parser ()
nextLine =
  do
    padding
    _ <- many1 newline
    spaces
    <?> "statement on next line"

-- manyLines line = sepBy (line <* padding) nextLine

lexeme :: Parser a -> Parser a
lexeme p = do
  padding
  x <- p
  padding
  return x

symbol :: String -> Parser String
symbol s = try (lexeme (string s))

parens :: Parser a -> Parser a
parens p = do
  _ <- symbol "("
  x <- p
  _ <- symbol ")"
  return x

identifier :: Parser String
identifier = try (many1 (letter <|> digit <|> char '_')) <?> "identifier"

variable :: Parser Expr
variable = Variable <$> identifier

integer :: Parser Expr
integer = IntValue . read <$> many1 digit

real :: Parser Expr
real = RealValue . read <$> many1 (digit <|> char '.')

stringLiteral :: Parser Expr
stringLiteral = do
  _ <- char '\"' <?> "\""
  str <- many (noneOf "\"")
  _ <- char '\"' <?> "\""
  return (StringLiteral str)

boolean :: Parser Expr
boolean = BoolLiteral <$> boolParser
  where
    boolParser = (symbol "TRUE" >> return True) <|> (symbol "FALSE" >> return False)

expr :: Parser Expr
expr = finalExpression <?> "valid expression"
  where
    finalExpression = notExpression `chainl1` andOp `chainl1` orOp
    notExpression = (notOp <*> notExpression) <|> logicalExpression
    logicalExpression = negExpression `chainl1` logicalOp
    negExpression = (negOp <*> negExpression) <|> expression
    expression = factor `chainl1` mulOp `chainl1` addOp `chainl1` modOp
    factor =
      try arrayIndex
        <|> parens expr
        <|> try real
        <|> integer
        <|> boolean
        <|> try function
        <|> variable
        <|> stringLiteral

    function = do
      var <- identifier
      _ <- symbol "("
      args <- sepBy expr (symbol ",")
      _ <- symbol ")"
      return $ Function var args

    arrayIndex = do
      var <- identifier
      _ <- symbol "["
      i <- index
      _ <- symbol "]"
      return $ ArrayIndex var i

    index = sepBy expr (symbol ",")

    modOp = symbol "MOD" >> return Mod

    mulOp =
      (symbol "*" >> return Mul)
        <|> (symbol "DIV" >> return IntDiv)
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
  _ <- symbol ":"
  Range start <$> integerLiteral

dimensionsParser :: Parser Dimension
dimensionsParser = rangeParser `sepBy` symbol ","

arrayParser :: Parser Type
arrayParser = do
  _ <- symbol "ARRAY"
  _ <- symbol "["
  dimensions <- dimensionsParser
  _ <- symbol "]"
  _ <- symbol "OF"
  Array <$> typeParser <*> pure dimensions

typeParser :: Parser Type
typeParser =
  (symbol "INTEGER" >> return IntType)
    <|> (symbol "REAL" >> return RealType)
    <|> (symbol "BOOLEAN" >> return BoolType)
    <|> (symbol "STRING" >> return StringType)
    <|> (symbol "CHAR" >> return CharType)
    <|> arrayParser
    <|> (UDT <$> identifier)

-- TODO: Multiple statements in one line
declareStmt :: Parser Stmt
declareStmt = do
  _ <- symbol "DECLARE"
  var <- identifier
  _ <- symbol ":"
  Declare var <$> typeParser

assignStmt :: Parser Stmt
assignStmt = do
  spaces
  var <- lValueParser
  _ <- symbol "<-" <|> fail ("missing <- in assignment: \n" ++ var ++ " <- ...")
  Assign var <$> expr

outputStmt :: Parser Stmt
outputStmt = do
  _ <- symbol "OUTPUT"
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
  _ <- symbol "INPUT"
  Input <$> lValueParser

ifElseStmt :: Parser Stmt
ifElseStmt = do
  _ <- symbol "IF"
  cond <- expr
  _ <- symbol "THEN" <|> fail "missing THEN in IF-ELSE statement"
  nextLine
  thenStmt <- many stmt
  _ <- symbol "ELSE" <|> fail "missing ELSE in IF-ELSE statement"
  nextLine
  elseStmt <- many stmt
  _ <- symbol "ENDIF" <|> fail "missing ENDIF in IF-ELSE statement"
  return $ Cond cond thenStmt elseStmt

ifStmt :: Parser Stmt
ifStmt = do
  _ <- symbol "IF"
  cond <- expr
  _ <- symbol "THEN" <|> fail "missing THEN in IF statement"
  nextLine
  spaces
  thenStmt <- many stmt
  _ <- symbol "ENDIF" <|> fail "missing ENDIF in IF statement"
  return $ Cond cond thenStmt []

condStmt :: Parser Stmt
condStmt = try ifElseStmt <|> ifStmt

whileStmt :: Parser Stmt
whileStmt = do
  _ <- symbol "WHILE"
  cond <- expr
  -- _ <- symbol "DO" <|> fail "missing DO in WHILE statement"
  nextLine
  body <- many stmt
  _ <- symbol "ENDWHILE" <|> fail "missing ENDWHILE in WHILE statement"
  return $ While cond body

forOneStmt :: Parser Stmt
forOneStmt = do
  _ <- symbol "FOR"
  var <- identifier
  _ <- symbol "<-"
  start <- expr
  _ <- symbol "TO"
  end <- expr
  _ <- symbol "DO" <|> fail "missing DO in FOR statement"
  nextLine
  body <- many stmt
  _ <- symbol "NEXT" <|> fail ("missing NEXT " ++ var ++ " in FOR statement")
  _ <- symbol var
  return $ For var start end (IntValue 1) body

forStepStmt :: Parser Stmt
forStepStmt = do
  _ <- symbol "FOR"
  var <- identifier
  _ <- symbol "<-"
  start <- expr
  _ <- symbol "TO"
  end <- expr
  _ <- symbol "STEP"
  step <- expr
  _ <- symbol "DO" <|> fail "missing DO in FOR statement"
  nextLine
  body <- many stmt
  _ <- symbol "NEXT" <|> fail ("missing NEXT " ++ var ++ " in FOR statement")
  _ <- symbol var
  return $ For var start end step body

forStmt :: Parser Stmt
forStmt = try forStepStmt <|> forOneStmt

repeatStmt :: Parser Stmt
repeatStmt = do
  _ <- symbol "REPEAT"
  nextLine
  body <- many stmt
  _ <- symbol "UNTIL" <|> fail "missing UNTIL in REPEAT statement"
  Repeat body <$> expr

commentStmt :: Parser Stmt
commentStmt = do
  _ <- symbol "//"
  Comment <$> many (noneOf "\n")

assignArrayStmt :: Parser Stmt
assignArrayStmt = do
  spaces
  var <- identifier
  _ <- symbol "["
  indices <- sepBy expr (symbol ",")
  _ <- symbol "]" <|> fail "missing closing ]"
  _ <- symbol "<-" <|> fail ("missing <- in assignment: \n" ++ var ++ "[...] <- ...")
  AssignArray var indices <$> expr

arg :: Parser (VarName, Type)
arg = do
  spaces
  var <- identifier
  _ <- symbol ":"
  dataType <- typeParser
  return (var, dataType)

returnStmt :: Parser Stmt
returnStmt = do
  _ <- symbol "RETURN"
  Return <$> expr

functionStmt :: Parser Stmt
functionStmt = do
  _ <- symbol "FUNCTION"
  name <- identifier <|> fail "missing identifier in FUNCTION definition"
  _ <- symbol "(" <|> fail "missing opening paren in FUNCTION definition"
  args <- sepBy arg (symbol ",")
  _ <- symbol ")" <|> fail "missing closing paren"
  _ <- symbol "RETURNS" <|> fail "missing RETURNS in FUNCTION definition"
  returnType <- typeParser <|> fail "missing return type"
  nextLine
  statements <- many stmt
  _ <- symbol "ENDFUNCTION" <|> fail "missing ENDFUNCTION in FUNCTION definition"
  return $ FunctionDef name args returnType statements

procedureStmt :: Parser Stmt
procedureStmt = do
  _ <- symbol "PROCEDURE"
  name <- identifier <|> fail "missing identifier in PROCEDURE definition"
  _ <- symbol "(" <|> fail "missing opening paren in PROCEDURE definition"
  args <- sepBy arg (symbol ",")
  _ <- symbol ")" <|> fail "missing closing paren"
  nextLine
  statements <- many stmt
  _ <- symbol "ENDPROCEDURE"
  return $ ProcedureDef name args statements

callStmt :: Parser Stmt
callStmt = do
  _ <- symbol "CALL"
  name <- identifier
  _ <- symbol "("
  args <- sepBy expr (symbol ",")
  _ <- symbol ")" <|> fail "missing closing paren"
  return $ Call name args

-- We simulate pseudocode constants (DECLARES) with immutable values
constantStmt :: Parser Stmt
constantStmt = do
  _ <- symbol "CONSTANT"
  name <- identifier
  _ <- symbol "="
  value <- try boolean <|> try stringLiteral <|> integer
  return $ Constant name value

stmt :: Parser Stmt
stmt =
  ( commentStmt
      <|> constantStmt
      <|> callStmt
      <|> returnStmt
      <|> forStmt
      <|> whileStmt
      <|> repeatStmt
      <|> declareStmt
      <|> condStmt
      <|> outputStmt
      <|> inputStmt
      <|> try assignStmt
      <|> try assignArrayStmt
  )
    <* nextLine

program :: Parser [Stmt]
program = spaces *> many line <* eof
  where
    line = (procedureStmt <* nextLine) <|> (functionStmt <* nextLine) <|> stmt

createExecutable :: String -> String -> String -> IO ()
createExecutable src dest out = do
  source <- readFile src
  case parse program "" (source ++ "\n") of
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