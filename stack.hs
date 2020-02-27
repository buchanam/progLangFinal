module Stack where

import Prelude
import Data.List

type Prog = [Cmd]

data Cmd = PushN Int
         | PushB Bool
         | PushS String
         | PushP Prog
         | Drop
         | Dup
         | Swap
         | Over
         | Rot
         | Add
         | Sub
         | Mul
         | Mod
         | Div
         | Equ
         | Gt
         | Lt
         | Concat
         | IfElse Prog Prog
         | Macro Prog
         | Loop
         | While Cmd
    deriving (Eq,Show)

data Block
        = I Int
        | B Bool
        | S String
        | P Prog
    deriving (Eq,Show)

type Stack = [Block]

-- I think when we do static typing we won't need the maybe
cmd :: Cmd -> Stack -> Stack
cmd (PushN i) s     = I i : s
cmd (PushB b) s     = B b : s
cmd (PushS c) s     = S c : s
cmd (PushP p) s     = P p : s
cmd Drop      s     = case s of
                        (a : s')         -> s'
cmd Dup       s     = case s of 
                        (a : s')         -> (a : a : s')
cmd Swap      s     = case s of
                        (a : b : s')     -> (b : a : s')
cmd Over      s     = case s of
                        (a : b : s')     -> (b : a : b : s')
cmd Rot       s     = case s of
                        (a : b : c : s') -> (b : c : a : s')
cmd Add       s     = case s of
                        (I a : I b : s') -> (I (a+b) : s')
cmd Sub       s     = case s of
                        (I a : I b : s') -> (I (b-a) : s')
cmd Mul       s     = case s of
                        (I a : I b : s') -> (I (a*b) : s')
cmd Mod       s     = case s of
                        (I a : I b : s') -> (I (b `mod` a) : s')
cmd Div       s     = case s of
                        (I c : I d : s') -> (I (d `div` c) : s')
cmd Equ       s     = case s of
                        (I a : I b : s') -> (B (a == b) : s')
                        (B c : B d : s') -> (B (c == d) : s')
                        (S e : S f : s') -> (B (e == f) : s')
cmd Gt        s     = case s of
                        (I a : I b : s') -> (B (a>b) : s')
cmd Lt        s     = case s of
                        (I a : I b : s') -> (B (a<b) : s')
cmd Concat    s     = case s of
                        (S a : S b : s') -> (S (b++a) : s')
cmd (IfElse t e) s  = case s of
                        (B True : s')    -> prog t s'
                        (B False : s')   -> prog e s' 
cmd Loop      s     = case s of
                        (P p : I a : I b : s') -> loop p s' [I a, I b]
cmd (While c) s     = case s of
                        (P p : b : s') -> while c p b s'

loop :: Prog -> Stack -> Stack -> Stack
loop cmds ds cs = case cs of
                    (I a : I b : cs') -> if a < b then (loop cmds (prog cmds ds) (I (a+1) : I b : cs') ) else ds


while :: Cmd -> Prog -> Block -> Stack -> Stack
while c p b s = case (cmd c ((head (cmd Dup s)) : (cmd Dup [b]))) of
                     (B False : b : cs') -> s
                     (B True  : b : cs') -> while c p b (prog p s)
                
prog :: Prog -> Stack -> Stack
prog [] s       = s
prog (c : cs) s    = case cmd c s of
                       s' -> prog cs s'
myLang :: [Cmd] -> Stack
myLang [] = []
myLang p = prog p []

-- good example Euclid's Algorithm
-- prog [Over, Over, Gt, IfElse [Swap] [], PushN 0, PushP [Dup, Rot, Mod], While Gt, Drop] [I 210, I 45]
