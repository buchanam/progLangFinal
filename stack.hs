module Stack where

import Prelude
import Data.List

type Prog = [Cmd]
type Stack = [Block]
type Macro = String
type Dict = [(String, Prog)]

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
         | Div
         | Equ
         | Gt
         | Lt
         | Concat
         | IfElse Prog Prog
         | Define Macro Prog
         | Call Macro
         | Loop
    deriving (Eq,Show)

data Block
        = I Int
        | B Bool
        | S String
        | P Prog
    deriving (Eq,Show)

-- I think when we do static typing we won't need the maybe
cmd :: Cmd -> Stack -> Dict -> (Stack, Dict)
cmd (PushN i) s  d  = ((I i : s), d)
cmd (PushB b) s  d  = ((B b : s), d)
cmd (PushS c) s  d  = ((S c : s), d)
cmd (PushP p) s  d  = ((P p : s), d)
cmd Drop      s  d  = case s of
                        (a : s')         -> (s', d)
cmd Dup       s  d  = case s of 
                        (a : s')         -> ((a : a : s'), d)
cmd Swap      s  d  = case s of
                        (a : b : s')     -> ((b : a : s'), d)
cmd Over      s  d  = case s of
                        (a : b : s')     -> ((a : b : a : s'), d)
cmd Rot       s  d  = case s of
                        (a : b : c : s') -> ((b : c : a : s'), d)
cmd Add       s  d  = case s of
                        (I a : I b : s') -> ((I (a+b) : s'), d)
cmd Sub       s  d  = case s of
                        (I a : I b : s') -> ((I (a-b) : s'), d)
cmd Mul       s  d  = case s of
                        (I a : I b : s') -> ((I (a*b) : s'), d)
cmd Div       s  d  = case s of
                        (I c : I g : s') -> ((I (c `div` g) : s'), d)
cmd Equ       s  d  = case s of
                        (I a : I b : s') -> ((B (a == b) : s'), d)
                        (B c : B g : s') -> ((B (c == g) : s'), d)
                        (S e : S f : s') -> ((B (e == f) : s'), d)
cmd Gt        s  d  = case s of
                        (I a : I b : s') -> ((B (a>b) : s'), d)
cmd Lt        s  d  = case s of
                        (I a : I b : s') -> ((B (a<b) : s'), d)
cmd Concat    s  d  = case s of
                        (S a : S b : s') -> ((S (a++b) : s'), d)
cmd (IfElse t e) s d = case s of
                        (B True : s')    -> prog t s' d
                        (B False : s')   -> prog e s' d
cmd Loop      s  d   = case s of
                        (P p : I a : I b : s') -> (loop p s' [I a, I b] d)
cmd (Define n p) s d = (s, ((n, p) : d))
cmd (Call n)  s  d  = case lookup n d of
                        Just p -> prog p s d
                        Nothing -> (s, d)

loop :: Prog -> Stack -> Stack -> Dict -> (Stack, Dict)
loop cmds ds cs d = case cs of
                    (I a : I b : cs') -> if a < b then case (prog cmds ds d) of
                                                        (ds', d') -> loop cmds ds' (I (a+1) : I b : cs') d 
                                                  else (ds, d)

prog :: Prog -> Stack -> Dict -> (Stack, Dict)
prog [] s d         = (s, d)
prog (c : cs) s d   = case cmd c s d of
                       (s', d') -> prog cs s' d'
                     


