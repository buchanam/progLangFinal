module Stack where

import Prelude
import Data.List

type Prog = [Cmd]
type Stack = [Block]
type Macro = String
type Dict = [(String, Prog)]

data Cmd = PushB Block
         | SOp StCmd
         | BOp BoolCmd
         | MOp ArCmd
         | COp CpCmd
         | Concat
         | IfElse Prog Prog
         | Define Macro Prog
         | Call Macro
         | While Cmd Block Prog
    deriving (Eq,Show)

data Block
        = I Int
        | B Bool
        | S String
    deriving (Eq,Show)

data StCmd
        = Drop
        | Dup
        | Swap
        | Over
        | Rot
    deriving (Eq,Show)

data BoolCmd
        = And
        | Or
        | Not
    deriving (Eq, Show)

data ArCmd
        = Add
        | Sub
        | Mul
        | Div
        | Mod
    deriving (Eq,Show)

data CpCmd
        = Equ
        | Gt
        | Lt
    deriving (Eq,Show)

-- Cmd Semantic Function
cmd :: Cmd -> Stack -> Dict -> (Stack, Dict)
cmd (PushB b) s  d  = ((b : s), d)
cmd (SOp c)   s  d  = case c of
                        Drop -> case s of
                                    (a : s')         -> (s', d)
                        Dup  -> case s of
                                    (a : s')         -> ((a : a : s'), d)
                        Swap -> case s of
                                    (a : b : s')     -> ((b : a : s'), d)
                        Over -> case s of
                                    (a : b : s')     -> ((b : a : b : s'), d)
                        Rot  -> case s of
                                    (a : b : c : s') -> ((b : c : a : s'), d)
cmd (MOp c)   s  d  = case c of
                        Add ->  case s of
                                    (I a : I b : s') -> ((I (a+b) : s'), d)
                        Sub ->  case s of
                                    (I a : I b : s') -> ((I (b-a) : s'), d)
                        Mul ->  case s of
                                    (I a : I b : s') -> ((I (a*b) : s'), d)
                        Div ->  case s of
                                    (I c : I g : s') -> ((I (g `div` c) : s'), d)
                        Mod ->  case s of
                                    (I a : I b : s') -> ((I (b `mod` a) : s'), d)
cmd (COp c)   s  d  = case c of
                        Equ ->  case s of
                                    (I a : I b : s') -> ((B (a == b) : s'), d)
                                    (B c : B g : s') -> ((B (c == g) : s'), d)
                                    (S e : S f : s') -> ((B (e == f) : s'), d)
                        Gt  ->  case s of
                                    (I a : I b : s') -> ((B (a>b) : s'), d)
                        Lt  ->  case s of
                                    (I a : I b : s') -> ((B (a<b) : s'), d)
cmd Concat    s  d  = case s of
                        (S a : S b : s') -> ((S (b++a) : s'), d)
cmd (IfElse t e) s d = case s of
                        (B True : s')    -> prog t s' d
                        (B False : s')   -> prog e s' d
cmd (While c b p) s  d   = while c p b s d
cmd (Define n p) s d = (s, ((n, p) : d))
cmd (Call n)  s  d  = case lookup n d of
                        Just p -> prog p s d
                        Nothing -> (s, d)
cmd (BOp c) s d     = case c of
                        And -> case s of 
                           (B True: B True:s') -> ((B True: s'), d)
                           (_:_: s')            -> ((B False: s'), d)
                        Or  -> case s of 
                           (B True: B True: s')  -> ((B True: s'), d)
                           (B True: B False: s') -> ((B True: s'), d)
                           (B False: B True: s') -> ((B True: s'), d)
                           (_: _: s')            -> ((B False: s'), d)
                        Not -> case s of 
                           (B True:s')  -> ((B False:s'), d)
                           (B False:s') -> ((B True:s'), d)


-- while helper function
while :: Cmd -> Prog -> Block -> Stack -> Dict -> (Stack, Dict)
while c p b s d = case ((cmd (SOp Dup) s d), (cmd (SOp Dup) [b] d)) of
                    ((s1, _), (s2, _)) -> case (cmd c ((head s1) : s2) d) of
                                              ((B False : b : cs'), d') -> (s, d')
                                              ((B True  : b : cs'), d') -> case (prog p s d') of
                                                (ds', d'') -> while c p b ds' d''

-- Semantic function for Prog
prog :: Prog -> Stack -> Dict -> (Stack, Dict)
prog [] s d         = (s, d)
prog (c : cs) s d   = case cmd c s d of
                       (s', d') -> prog cs s' d'

stackm :: [Cmd] -> Stack
stackm [] = []
stackm p = case (prog p [] []) of
             (s, d) -> s

