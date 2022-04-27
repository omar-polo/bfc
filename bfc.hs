-- This is free and unencumbered software released into the public domain.
--
-- Anyone is free to copy, modify, publish, use, compile, sell, or
-- distribute this software, either in source code form or as a compiled
-- binary, for any purpose, commercial or non-commercial, and by any
-- means.
--
-- In jurisdictions that recognize copyright laws, the author or authors
-- of this software dedicate any and all copyright interest in the
-- software to the public domain. We make this dedication for the benefit
-- of the public at large and to the detriment of our heirs and
-- successors. We intend this dedication to be an overt act of
-- relinquishment in perpetuity of all present and future rights to this
-- software under copyright law.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
-- OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
-- ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.
--
-- For more information, please refer to <http://unlicense.org/>

module Main where

import Text.Printf
import System.Environment as E

data Token = Plus
           | Minus
           | Lesser
           | Greater
           | Point
           | Comma
           | BracketOpen
           | BracketClose
           deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize (x:xs) =
  let table = [ ('-', Minus), ('+', Plus)
              , ('<', Lesser), ('>', Greater)
              , ('.', Point), (',', Comma)
              , ('[', BracketOpen), (']', BracketClose) ]
      token = lookup x table in
    case token of
      Just x  -> x:(tokenize xs)
      Nothing -> tokenize xs
tokenize _ = []

data Expr = Inc
          | Dec
          | ShiftLeft
          | ShiftRight
          | Input
          | Output
          | Loop [Expr]
          deriving (Eq, Show)

type AST = [Expr]

data State = State ([AST], [Token]) deriving (Show)

initialState tokens = State ([[]], tokens)

finalize (State ([x], _)) = Just x
finalize _ = Nothing

simplexpr :: Expr -> [AST] -> [Token] -> State
simplexpr e (x:xs) tokens = State ((x ++ [e]):xs, tokens)

translate :: Token -> Expr
translate x = case x of
                Plus    -> Inc
                Minus   -> Dec
                Lesser  -> ShiftLeft
                Greater -> ShiftRight
                Point   -> Output
                Comma   -> Input

parser :: State -> Maybe State
parser (State (stack, (x:xs))) =
  case x of
    BracketOpen -> parser $ State ([]:stack, xs)
    BracketClose -> case stack of
                      (y:ys) -> parser $ simplexpr (Loop y) (ys) xs
                      _      -> Nothing
    _ -> parser $ simplexpr (translate x) stack xs
parser state = Just state

parse :: [Token] -> Maybe AST
parse tokens = parser (initialState tokens) >>= finalize

prologue = "export function w $main() {\n" ++
           "@start\n" ++
           "    %.1 =l alloc8 8\n" ++
           "    storel $tape, %.1"
epilogue = "    ret 0\n" ++
           "}\n" ++
           "data $tape = align 8 { z 4096 }"

-- subset of QBE that I need to convert the AST to
data Instruction = StoreW (Int, Int)        -- storew a, b
                 | StoreL (Int, Int)        -- storel a, b
                 | LoadW (Int, Int)         -- a =w loadw b
                 | LoadL (Int, Int)         -- a =w loadl b
                 | AddW (Int, Int, Int)     -- a =w add b, c
                 | AddL (Int, Int, Int)     -- a =l add b, c
                 | SubW (Int, Int, Int)     -- a =w sub b, c
                 | SubL (Int, Int, Int)     -- a =l sub b, c
                 | Call0 (Int, String)      -- a =w call $b()
                 | Call1 (Int, String, Int) -- a =w call $b(w c)
                 | Jmp (Int)                -- jmp a
                 | Jnz (Int, Int, Int)      -- jnz a, @loop.b, @loop.c
                 | Label (Int)              -- @loop.a
  deriving (Eq)

instance Show Instruction where
  show x =
    case x of
      StoreW (a, b)    -> printf "    storew %%.%d, %%.%d" a b
      StoreL (a, b)    -> printf "    storel %%.%d, %%.%d" a b
      LoadW (a, b)     -> printf "    %%.%d =w loadw %%.%d" a b
      LoadL (a, b)     -> printf "    %%.%d =l loadl %%.%d" a b
      AddW (a, b, c)   -> printf "    %%.%d =w add %%.%d, %d" a b c
      AddL (a, b, c)   -> printf "    %%.%d =l add %%.%d, %d" a b c
      SubW (a, b, c)   -> printf "    %%.%d =w sub %%.%d, %d" a b c
      SubL (a, b, c)   -> printf "    %%.%d =l sub %%.%d, %d" a b c
      Call0 (a, fn)    -> printf "    %%.%d =w call $%s()" a fn
      Call1 (a, fn, b) -> printf "    %%.%d =w call $%s(w %%.%d)" a fn b
      Jmp (a)          -> printf "    jmp @loop.%d" a
      Jnz (a, b, c)    -> printf "    jnz %%.%d, @loop.%d, @loop.%d" a b c
      Label (a)        -> printf "@loop.%d" a

-- I'm keeping the pointer to the current cell in the "%.1"
-- intermediary.  It's always there because qbe allows to
--	storel %.X %.1
-- even if it's a SSA.
cell = 1

compile' :: Int -> Int -> [AST] -> [[Instruction]] -> [Instruction]
compile' n h ((x:xs):ys) trail =
  case x of
    Inc -> LoadL(n+1, cell)  :
           LoadW(n+2, n+1)   :
           AddW(n+3, n+2, 1) :
           StoreW(n+3, n+1)  :
           compile' (n+3) h (xs:ys) trail
    Dec -> LoadL(n+1, cell)  :
           LoadW(n+2, n+1)   :
           SubW(n+3, n+2, 1) :
           StoreW(n+3, n+1)  :
           compile' (n+3) h (xs:ys) trail
    ShiftLeft -> LoadL(n+1, cell)  :
                 SubL(n+2, n+1, 4) :
                 StoreL(n+2, cell) :
                 compile' (n+2) h (xs:ys) trail
    ShiftRight -> LoadL(n+1, cell)  :
                  AddL(n+2, n+1, 4) :
                  StoreL(n+2, cell) :
                  compile' (n+2) h (xs:ys) trail
    Input -> Call0(n+1, "getchar") :
             LoadL(n+2, cell)      :
             StoreW(n+1, n+2)      :
             compile' (n+2) h (xs:ys) trail
    Output -> LoadL(n+1, cell)           :
              LoadW(n+2, n+1)            :
              Call1(n+3, "putchar", n+2) :
              compile' (n+3) h (xs:ys) trail
    Loop (ast) -> Label(h)           :
                  LoadL(n+1, cell)   :
                  LoadW(n+2, n+1)    :
                  Jnz(n+2, h+1, h+2) :
                  Label(h+1)         :
                  compile' (n+3) (h+3) (ast:(xs:ys)) ([Jmp(h), Label(h+2)]:trail)
compile' n h ([]:ys) (t:ts) = t ++ (compile' n h ys ts)
compile' _ _ _ _ = []

compile ast = compile' 1 1 [ast] []

compileProg program = do
  let t = parse $ tokenize program in
    case t of
      Just ast -> do putStrLn prologue
                     mapM_ print (compile ast)
                     putStrLn epilogue
      Nothing  -> error "Compilation failed"

parseArgs [] = getContents
parseArgs path = concat `fmap` mapM readFile path

main = E.getArgs >>= parseArgs >>= compileProg
